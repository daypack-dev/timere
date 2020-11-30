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
  "[{syear} {smon:Xxx} {smday:0X} {shour:0X}:{smin:0X}:{ssec:0X}, {eyear} \
   {emon:Xxx} {emday:0X} {ehour:0X}:{emin:0X}:{esec:0X})"

let sexp_of_month x = CCSexp.atom @@ abbreviated_string_of_month x

let sexp_of_weekday x = CCSexp.atom @@ abbreviated_string_of_weekday x

let sexp_of_int x = CCSexp.atom @@ string_of_int x

let sexp_list_of_ints l = List.map sexp_of_int l

let sexp_of_date_time x =
  CCSexp.atom
  @@ Result.get_ok
  @@ sprintf_date_time default_date_time_format_string x

let sexp_of_range ~(f : 'a -> CCSexp.t) (r : 'a Time.Range.range) =
  match r with
  | `Range_inc (x, y) -> CCSexp.(list [ atom "range_inc"; f x; f y ])
  | `Range_exc (x, y) -> CCSexp.(list [ atom "range_exc"; f x; f y ])

let sexp_of_pattern (pat : Time.Pattern.pattern) : CCSexp.t =
  let years = sexp_list_of_ints pat.years in
  let months = List.map sexp_of_month pat.months in
  let month_days = sexp_list_of_ints pat.month_days in
  let weekdays = List.map sexp_of_weekday pat.weekdays in
  let hours = sexp_list_of_ints pat.hours in
  let minutes = sexp_list_of_ints pat.minutes in
  let seconds = sexp_list_of_ints pat.seconds in
  let timestamps =
    pat.timestamps
    |> List.map (fun x ->
        Result.get_ok @@ sprintf_timestamp default_date_time_format_string x)
    |> List.map CCSexp.atom
  in
  let open CCSexp in
  [
    Some (atom "pattern");
    (match years with [] -> None | _ -> Some (list (atom "years" :: years)));
    (match months with [] -> None | _ -> Some (list (atom "months" :: months)));
    ( match month_days with
      | [] -> None
      | _ -> Some (list (atom "month_days" :: month_days)) );
    ( match weekdays with
      | [] -> None
      | _ -> Some (list (atom "weekdays" :: weekdays)) );
    (match hours with [] -> None | _ -> Some (list (atom "hours" :: hours)));
    ( match minutes with
      | [] -> None
      | _ -> Some (list (atom "minutes" :: minutes)) );
    ( match seconds with
      | [] -> None
      | _ -> Some (list (atom "seconds" :: seconds)) );
    ( match timestamps with
      | [] -> None
      | _ -> Some (list (atom "timestamps" :: timestamps)) );
  ]
  |> List.filter_map (fun x -> x)
  |> list

let sexp_of_branching (b : Time.branching) : CCSexp.t =
  let open Time in
  let years = List.map (sexp_of_range ~f:sexp_of_int) b.years in
  let months = List.map (sexp_of_range ~f:sexp_of_month) b.months in
  let days =
    match b.days with
    | Month_days days -> (
        match days with
        | [] -> []
        | _ ->
          CCSexp.atom "month_days"
          :: List.map (sexp_of_range ~f:sexp_of_int) days )
    | Weekdays days -> (
        match days with
        | [] -> []
        | _ ->
          CCSexp.atom "weekdays"
          :: List.map (sexp_of_range ~f:sexp_of_weekday) days )
  in
  let hmss =
    List.map
      (sexp_of_range ~f:(fun { hour; minute; second } ->
           CCSexp.atom (Printf.sprintf "%d:%d:%d" hour minute second)))
      b.hmss
  in
  let open CCSexp in
  [
    Some (atom "branching");
    (match years with [] -> None | _ -> Some (list (atom "years" :: years)));
    (match months with [] -> None | _ -> Some (list (atom "months" :: months)));
    (match days with [] -> None | _ -> Some (list days));
    (match hmss with [] -> None | _ -> Some (list (atom "hmss " :: hmss)));
  ]
  |> List.filter_map (fun x -> x)
  |> list

let sexp_list_of_unary_op (op : Time.unary_op) =
  let open Time in
  match op with
  | Not -> [ CCSexp.atom "not" ]
  | Every -> [ CCSexp.atom "every" ]
  | Skip_n_points n ->
    [ CCSexp.atom "skip_n_points"; CCSexp.atom (string_of_int n) ]
  | Skip_n_intervals n ->
    [ CCSexp.atom "skip_n"; CCSexp.atom (string_of_int n) ]
  | Next_n_points n ->
    [ CCSexp.atom "next_n_points"; CCSexp.atom (string_of_int n) ]
  | Next_n_intervals n ->
    [ CCSexp.atom "next_n"; CCSexp.atom (string_of_int n) ]
  | Chunk { chunk_size; drop_partial } ->
    CCSexp.atom "chunk"
    :: CCSexp.atom (Int64.to_string chunk_size)
    :: (if drop_partial then [ CCSexp.atom "drop_partial" ] else [])
  | Shift n -> [ CCSexp.atom "shift"; CCSexp.atom (Int64.to_string n) ]
  | Lengthen n -> [ CCSexp.atom "lengthen"; CCSexp.atom (Int64.to_string n) ]
  | Tz_offset_s n ->
    [ CCSexp.atom "tz_offset_s"; CCSexp.atom (string_of_int n) ]

let to_sexp (t : Time.t) : CCSexp.t =
  let open Time in
  let rec aux t =
    match t with
    | Timestamp_interval_seq (_, s) ->
      let l =
        s
        |> List.of_seq
        |> List.map (fun (x, y) ->
            ( Result.get_ok
              @@ sprintf_timestamp default_date_time_format_string x,
              Result.get_ok
              @@ sprintf_timestamp default_date_time_format_string y ))
        |> List.map (fun (x, y) -> CCSexp.(list [ atom x; atom y ]))
      in
      CCSexp.list (CCSexp.atom "intervals" :: l)
    | Pattern (_, pat) -> sexp_of_pattern pat
    | Branching (_, b) -> sexp_of_branching b
    | Unary_op (_, op, t) -> CCSexp.list (sexp_list_of_unary_op op @ [ aux t ])
    | Binary_op (_, op, t1, t2) ->
      CCSexp.list
        [
          ( match op with
            | Union -> CCSexp.atom "union"
            | Inter -> CCSexp.atom "inter" );
          aux t1;
          aux t2;
        ]
    | Interval_inc (_, dt1, dt2) ->
      let open CCSexp in
      list
        [ atom "interval_inc"; sexp_of_date_time dt1; sexp_of_date_time dt2 ]
    | Interval_exc (_, dt1, dt2) ->
      let open CCSexp in
      list
        [ atom "interval_exc"; sexp_of_date_time dt1; sexp_of_date_time dt2 ]
    | Round_robin_pick_list (_, l) ->
      CCSexp.(list (atom "round_robin" :: List.map aux l))
    | Merge_list (_, l) -> CCSexp.(list (atom "merge" :: List.map aux l))
  in
  aux t

let pp_sexp formatter t = CCSexp.pp formatter (to_sexp t)
