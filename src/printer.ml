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

  let date_time_inner (date_time : Time.Date_time.t) : (string, unit) t =
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
        string "unix"
        >> return (Int64.to_string (Time.Date_time.to_timestamp date_time));
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
        attempt (char '{')
        >> Format_string_parsers.date_time_inner date_time
           << char '}';
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

let sprintf_timestamp ?(display_using_tz_offset_s = 0) format (time : int64) :
  (string, string) result =
  match
    Time.Date_time.of_timestamp
      ~tz_offset_s_of_date_time:display_using_tz_offset_s time
  with
  | Error () -> Error "Invalid unix second"
  | Ok dt -> sprintf_date_time format dt

let pp_timestamp ?(display_using_tz_offset_s = 0) format formatter x =
  match sprintf_timestamp ~display_using_tz_offset_s format x with
  | Error msg -> invalid_arg msg
  | Ok s -> Format.fprintf formatter "%s" s

let sprintf_interval ?(display_using_tz_offset_s = 0) (format : string)
    ((s, e) : Time.Interval.t) : (string, string) result =
  let open MParser in
  let open Parser_components in
  let single (start_date_time : Time.Date_time.t)
      (end_date_time : Time.Date_time.t) : (string, unit) t =
    choice
      [
        attempt (string "{{" >> return "{");
        ( attempt (char '{')
          >> ( attempt (char 's' >> return start_date_time)
               <|> (char 'e' >> return end_date_time) )
          >>= fun date_time ->
          Format_string_parsers.date_time_inner date_time << char '}' );
        ( many1_satisfy (function '{' -> false | _ -> true)
          >>= fun s -> return s );
      ]
  in
  let p (start_date_time : Time.Date_time.t) (end_date_time : Time.Date_time.t)
    : (string list, unit) t =
    many (single start_date_time end_date_time)
  in
  match
    Time.Date_time.of_timestamp
      ~tz_offset_s_of_date_time:display_using_tz_offset_s s
  with
  | Error () -> Error "Invalid start unix time"
  | Ok s -> (
      match
        Time.Date_time.of_timestamp
          ~tz_offset_s_of_date_time:display_using_tz_offset_s e
      with
      | Error () -> Error "Invalid end unix time"
      | Ok e ->
        parse_string
          ( p s e
            >>= fun s ->
            get_pos
            >>= fun pos ->
            attempt eof
            >> return s
               <|> fail
                 (Printf.sprintf "Expected EOI, pos: %s" (string_of_pos pos))
          )
          format ()
        |> result_of_mparser_result
        |> Result.map (fun l -> String.concat "" l) )

let pp_interval ?(display_using_tz_offset_s = 0) format formatter interval =
  match sprintf_interval ~display_using_tz_offset_s format interval with
  | Error msg -> invalid_arg msg
  | Ok s -> Format.fprintf formatter "%s" s

let sprint_duration ({ days; hours; minutes; seconds } : Duration.t) : string =
  if days > 0 then
    Printf.sprintf "%d days %d hours %d mins %d secs" days hours minutes seconds
  else if hours > 0 then
    Printf.sprintf "%d hours %d mins %d secs" hours minutes seconds
  else if minutes > 0 then Printf.sprintf "%d mins %d secs" minutes seconds
  else Printf.sprintf "%d secs" seconds

let pp_duration formatter x = Format.fprintf formatter "%s" (sprint_duration x)

let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {hour:0X}:{min:0X}:{sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {shour:0X}:{smin:0X}:{ssec:0X}, \
   {eyear} {emon:Xxx} {emday:0X} {ehour:0X}:{emin:0X}:{esec:0X})"

let to_sexp (t : Time.t) : CCSexp.t =
  let open Time in
  let rec aux t =
    match t with
    | Timestamp_interval_seq (_, s) ->
      s
      |> List.of_seq
      |> List.map (fun x -> Result.get_ok @@ sprintf_interval default_interval_format_string x)
      |> List.map CCSexp.atom
      |> CCSexp.list
    | _ -> failwith "Unimplemented"
  in
  aux t
