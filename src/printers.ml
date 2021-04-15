open Date_time_components

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

exception Date_time_cannot_deduce_tz_offset_s of Time.Date_time'.t

module Format_string_parsers = struct
  open MParser
  open Parser_components

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
      (satisfy (fun _ -> true)
       >>= fun padding -> char 'X' >> return (Some padding))
    <|> (char 'X' >> return None)

  let date_time_inner (date_time : Time.Date_time'.t) : (string, unit) t =
    let tz_offset_s =
      match date_time.tz_info with
      | `Tz_only _ -> None
      | `Tz_offset_s_only x | `Tz_and_tz_offset_s (_, x) -> Some x
    in
    choice
      [
        attempt (string "year") >> return (Printf.sprintf "%04d" date_time.year);
        attempt (string "mon:")
        >> (attempt size_and_casing
            >>= (fun x ->
                return
                  (map_string_to_size_and_casing x
                     (Time.full_string_of_month date_time.month)))
                <|> (padding
                     >>= fun padding ->
                     return (pad_int padding (human_int_of_month date_time.month))));
        (attempt (string "mday:")
         >> padding
         >>= fun padding -> return (pad_int padding date_time.day));
        (attempt (string "wday:")
         >> size_and_casing
         >>= fun x ->
         match
           weekday_of_month_day ~year:date_time.year ~month:date_time.month
             ~mday:date_time.day
         with
         | None -> fail "Invalid date time"
         | Some wday ->
           return
             (map_string_to_size_and_casing x
                (Time.full_string_of_weekday wday)));
        (attempt (string "hour:")
         >> padding
         >>= fun padding -> return (pad_int padding date_time.hour));
        (attempt (string "12hour:")
         >> padding
         >>= fun padding ->
         let hour = if date_time.hour = 0 then 12 else date_time.hour mod 12 in
         return (pad_int padding hour));
        (attempt (string "min:")
         >> padding
         >>= fun padding -> return (pad_int padding date_time.minute));
        (attempt (string "sec:")
         >> padding
         >>= fun padding -> return (pad_int padding date_time.second));
        attempt (string "ns") >> return (string_of_int date_time.ns);
        (attempt (string "sec-frac:")
         >> nat_zero
         >>= fun precision ->
         if precision = 0 then fail "Precision cannot be 0"
         else
           let ns = float_of_int date_time.ns in
           let precision' = float_of_int precision in
           let frac =
             ns *. (10. ** precision') /. Span.ns_count_in_s_float
             |> CCFloat.round
             |> int_of_float
           in
           return (Printf.sprintf "%0*d" precision frac));
        (attempt (string "tzoff-sign")
         >>= fun _ ->
         match tz_offset_s with
         | None -> raise (Date_time_cannot_deduce_tz_offset_s date_time)
         | Some tz_offset_s ->
           if tz_offset_s >= 0 then return "+" else return "-");
        (attempt (string "tzoff-hour:")
         >> padding
         >>= fun padding ->
         match tz_offset_s with
         | None -> raise (Date_time_cannot_deduce_tz_offset_s date_time)
         | Some tz_offset_s ->
           let d = Duration.make ~seconds:(abs tz_offset_s) () in
           return (pad_int padding Duration.(d.hours)));
        (attempt (string "tzoff-min:")
         >> padding
         >>= fun padding ->
         match tz_offset_s with
         | None -> raise (Date_time_cannot_deduce_tz_offset_s date_time)
         | Some tz_offset_s ->
           let d = Duration.make ~seconds:(abs tz_offset_s) () in
           return (pad_int padding Duration.(d.minutes)));
        (attempt (string "tzoff-sec:")
         >> padding
         >>= fun padding ->
         match tz_offset_s with
         | None -> raise (Date_time_cannot_deduce_tz_offset_s date_time)
         | Some tz_offset_s ->
           let d = Duration.make ~seconds:(abs tz_offset_s) () in
           return (pad_int padding Duration.(d.seconds)));
        (* string "unix"
         * >> return (Int64.to_string (Time.Date_time'.to_timestamp date_time)); *)
      ]
end

let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {hour:0X}:{min:0X}:{sec:0X} \
   {tzoff-sign}{tzoff-hour:0X}:{tzoff-min:0X}:{tzoff-sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {shour:0X}:{smin:0X}:{ssec:0X} \
   {stzoff-sign}{stzoff-hour:0X}:{stzoff-min:0X}:{stzoff-sec:0X}, {eyear} \
   {emon:Xxx} {emday:0X} {ehour:0X}:{emin:0X}:{esec:0X} \
   {etzoff-sign}{etzoff-hour:0X}:{etzoff-min:0X}:{etzoff-sec:0X})"

exception Invalid_format_string of string

let invalid_format_string s = raise (Invalid_format_string s)

let pp_date_time ?(format : string = default_date_time_format_string) ()
    (formatter : Format.formatter) (x : Time.Date_time'.t) : unit =
  let open MParser in
  let open Parser_components in
  let single formatter (date_time : Time.Date_time'.t) : (unit, unit) t =
    choice
      [
        attempt (string "{{" |>> fun _ -> Fmt.pf formatter "{");
        (attempt (char '{')
         >> (Format_string_parsers.date_time_inner date_time << char '}')
         |>> fun s -> Fmt.pf formatter "%s" s);
        (many1_satisfy (function '{' -> false | _ -> true)
         |>> fun s -> Fmt.pf formatter "%s" s);
      ]
  in
  let p formatter (date_time : Time.Date_time'.t) : (unit, unit) t =
    many (single formatter date_time) >> return ()
  in
  match
    result_of_mparser_result @@ parse_string (p formatter x << eof) format ()
  with
  | Error msg -> invalid_format_string msg
  | Ok () -> ()

let string_of_date_time ?(format : string = default_date_time_format_string)
    (x : Time.Date_time'.t) : string option =
  try Some (Fmt.str "%a" (pp_date_time ~format ()) x)
  with Date_time_cannot_deduce_tz_offset_s _ -> None

let pp_timestamp ?(display_using_tz = Time_zone.utc)
    ?(format = default_date_time_format_string) () formatter time =
  match Time.Date_time'.of_timestamp ~tz_of_date_time:display_using_tz time with
  | None -> invalid_arg "Invalid unix second"
  | Some dt -> Fmt.pf formatter "%a" (pp_date_time ~format ()) dt

let string_of_timestamp ?(display_using_tz = Time_zone.utc)
    ?(format = default_date_time_format_string) (time : Span.t) : string =
  Fmt.str "%a" (pp_timestamp ~display_using_tz ~format ()) time

let pp_hms formatter (hms : Time.hms) : unit =
  Fmt.pf formatter "%d:%d:%d" hms.hour hms.minute hms.second

let string_of_hms hms = Fmt.str "%a" pp_hms hms

let pp_interval ?(display_using_tz = Time_zone.utc)
    ?(format = default_interval_format_string) () formatter
    ((s, e) : Time.Interval'.t) : unit =
  let open MParser in
  let open Parser_components in
  let single (start_date_time : Time.Date_time'.t)
      (end_date_time : Time.Date_time'.t) : (unit, unit) t =
    choice
      [
        attempt (string "{{" |>> fun _ -> Fmt.pf formatter "{");
        (attempt (char '{')
         >> (attempt (char 's' >> return start_date_time)
             <|> (char 'e' >> return end_date_time))
         >>= fun date_time ->
         Format_string_parsers.date_time_inner date_time
         << char '}'
         |>> fun s -> Fmt.pf formatter "%s" s);
        (many1_satisfy (function '{' -> false | _ -> true)
         |>> fun s -> Fmt.pf formatter "%s" s);
      ]
  in
  let p (start_date_time : Time.Date_time'.t)
      (end_date_time : Time.Date_time'.t) : (unit, unit) t =
    many (single start_date_time end_date_time) >> return ()
  in
  match Time.Date_time'.of_timestamp ~tz_of_date_time:display_using_tz s with
  | None -> invalid_arg "Invalid start unix time"
  | Some s -> (
      match
        Time.Date_time'.of_timestamp ~tz_of_date_time:display_using_tz e
      with
      | None -> invalid_arg "Invalid end unix time"
      | Some e -> (
          match
            result_of_mparser_result
            @@ parse_string
              (p s e
               >>= fun s ->
               get_pos
               >>= fun pos ->
               attempt eof
               >> return s
                  <|> fail
                    (Printf.sprintf "Expected EOI, pos: %s"
                       (string_of_pos pos)))
              format ()
          with
          | Error msg -> invalid_format_string msg
          | Ok () -> ()))

let string_of_interval ?(display_using_tz = Time_zone.utc)
    ?(format : string = default_interval_format_string)
    (interval : Time.Interval'.t) : string =
  Fmt.str "%a" (pp_interval ~display_using_tz ~format ()) interval

let pp_intervals ?(display_using_tz = Time_zone.utc)
    ?(format = default_interval_format_string) ?(sep = Fmt.cut) () formatter
    intervals =
  Fmt.seq ~sep (pp_interval ~display_using_tz ~format ()) formatter intervals

let pp_span formatter ({ s; ns } : Span.t) : unit =
  Fmt.pf formatter "%Ld s + %d ns" s ns

let string_of_span (x : Span.t) : string = Fmt.str "%a" pp_span x

let pp_duration formatter ({ days; hours; minutes; seconds } : Duration.t) :
  unit =
  if days > 0 then
    Fmt.pf formatter "%d days %d hours %d mins %d secs" days hours minutes
      seconds
  else if hours > 0 then
    Fmt.pf formatter "%d hours %d mins %d secs" hours minutes seconds
  else if minutes > 0 then Fmt.pf formatter "%d mins %d secs" minutes seconds
  else Fmt.pf formatter "%d secs" seconds

let string_of_duration (x : Duration.t) : string = Fmt.str "%a" pp_duration x

let wrap_to_sexp_into_pp_sexp (f : 'a -> CCSexp.t) :
  Format.formatter -> 'a -> unit =
  fun formatter x -> CCSexp.pp formatter (f x)

let pp_sexp = wrap_to_sexp_into_pp_sexp To_sexp.to_sexp
