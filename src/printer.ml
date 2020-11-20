type case =
  | Upper
  | Lower

type size_and_casing =
  | Abbreviated of case * case * case
  | Full of case * case

let map_char_to_case (case : case) (c : char) =
  match case with
  | Upper -> Char.uppercase_ascii c
  | Lower -> Char.lowercase_ascii c

let map_string_to_size_and_casing (x : size_and_casing) (s : string) : string =
  match x with
  | Abbreviated (case1, case2, case3) ->
    let c1 = map_char_to_case case1 s.[0] in
    let c2 = map_char_to_case case2 s.[1] in
    let c3 = map_char_to_case case3 s.[2] in
    Printf.sprintf "%c%c%c" c1 c2 c3
  | Full (case1, case2) ->
    String.mapi
      (fun i c ->
         if i = 0 then map_char_to_case case1 c else map_char_to_case case2 c)
      s

let pad_int (c : char option) (x : int) : string =
  match c with
  | None -> string_of_int x
  | Some c -> if x < 10 then Printf.sprintf "%c%d" c x else string_of_int x

let full_string_of_weekday (wday : Time.weekday) : string =
  match wday with
  | `Sun -> "Sunday"
  | `Mon -> "Monday"
  | `Tue -> "Tuesday"
  | `Wed -> "Wednesday"
  | `Thu -> "Thursday"
  | `Fri -> "Friday"
  | `Sat -> "Saturday"

let abbreviated_string_of_weekday (wday : Time.weekday) : string =
  String.sub (full_string_of_weekday wday) 0 3

let full_string_of_month (month : Time.month) : string =
  match month with
  | `Jan -> "January"
  | `Feb -> "February"
  | `Mar -> "March"
  | `Apr -> "April"
  | `May -> "May"
  | `Jun -> "June"
  | `Jul -> "July"
  | `Aug -> "August"
  | `Sep -> "September"
  | `Oct -> "October"
  | `Nov -> "November"
  | `Dec -> "December"

let abbreviated_string_of_month (month : Time.month) : string =
  String.sub (full_string_of_month month) 0 3

module Format_string_parsers = struct
  open MParser

  let case : (case, unit) t =
    attempt (char 'x' >> return Lower) <|> (char 'X' >> return Upper)

  let size_and_casing : (size_and_casing, unit) t =
    case
    >>= fun c1 ->
    case
    >>= fun c2 ->
    attempt (char '*' >> return (Full (c1, c2)))
    <|> (case >>= fun c3 -> return (Abbreviated (c1, c2, c3)))

  let padding : (char option, unit) t =
    attempt
      ( satisfy (fun _ -> true)
        >>= fun padding -> char 'X' >> return (Some padding) )
    <|> (char 'X' >> return None)

  let inner (date_time : Time.Date_time.t) : (string, unit) t =
    choice
      [
        attempt (string "year") >> return (string_of_int date_time.year);
        ( attempt (string "mon:")
          >> size_and_casing
          >>= fun x ->
          return
            (map_string_to_size_and_casing x
               (full_string_of_month date_time.month)) );
        ( attempt (string "mday:")
          >> padding
          >>= fun padding -> return (pad_int padding date_time.day) );
        ( attempt (string "wday:")
          >> size_and_casing
          >>= fun x ->
          match
            Time.weekday_of_month_day ~year:date_time.year ~month:date_time.month
              ~mday:date_time.day
          with
          | Error () -> fail "Invalid date time"
          | Ok wday ->
            return
              (map_string_to_size_and_casing x (full_string_of_weekday wday)) );
        attempt
          ( string "hour:"
            >> padding
            >>= fun padding -> return (pad_int padding date_time.hour) );
        attempt
          ( string "12hour:"
            >> padding
            >>= fun padding ->
            let hour = if date_time.hour = 0 then 12 else date_time.hour mod 12 in
            return (pad_int padding hour) );
        attempt
          ( string "min:"
            >> padding
            >>= fun padding -> return (pad_int padding date_time.minute) );
        attempt
          ( string "sec:"
            >> padding
            >>= fun padding -> return (pad_int padding date_time.second) );
        ( string "unix"
          >>
          match Time.Date_time.to_timestamp date_time with
          | Error () -> fail "Invalid date time"
          | Ok sec -> return (Int64.to_string sec) );
      ]
end

let sprintf_date_time (format : string) (x : Time.Date_time.t) :
  (string, string) result =
  let open MParser in
  let open Parser_components in
  let single (date_time : Time.Date_time.t) : (string, unit) t =
    choice
      [
        attempt (string "{{" >> return "{");
        attempt (char '{') >> Format_string_parsers.inner date_time << char '}';
        (many1_satisfy (function '{' -> false | _ -> true) |>> fun s -> s);
      ]
  in
  let p (date_time : Time.Date_time.t) : (string list, unit) t =
    many (single date_time)
  in
  parse_string (p x << eof) format ()
  |> result_of_mparser_result
  |> Result.map (fun l -> String.concat "" l)

let pp_date_time format formatter x =
  match sprintf_date_time format x with
  | Error msg -> invalid_arg msg
  | Ok s -> Format.fprintf formatter "%s" s
