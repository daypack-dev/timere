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

exception Date_time_cannot_deduce_offset_from_utc of Date_time.t

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

  let date_time_inner (date_time : Date_time.t) : (string, unit) t =
    let single_offset =
      match date_time.offset_from_utc with
      | `Single offset -> Some (Span.For_human'.view offset)
      | `Ambiguous _ -> None
    in
    let Date.Ymd_date.{ year; month; day } = Date_time.ymd_date date_time in
    let weekday = Date_time.weekday date_time in
    let Time.{ hour; minute; second; ns } = Date_time.time_view date_time in
    choice
      [
        attempt (string "year") >> return (Printf.sprintf "%04d" year);
        attempt (string "mon:")
        >> (attempt size_and_casing
            >>= (fun x ->
                return
                  (map_string_to_size_and_casing x
                     (CCOpt.get_exn_or "Expected valid month"
                      @@ full_string_of_month month)))
                <|> (padding >>= fun padding -> return (pad_int padding month)));
        (attempt (string "day:")
         >> padding
         >>= fun padding -> return (pad_int padding day));
        (attempt (string "wday:")
         >> size_and_casing
         >>= fun x ->
         return
           (map_string_to_size_and_casing x (full_string_of_weekday weekday)));
        (attempt (string "hour:")
         >> padding
         >>= fun padding -> return (pad_int padding hour));
        (attempt (string "12hour:")
         >> padding
         >>= fun padding ->
         let hour = if hour = 0 then 12 else hour mod 12 in
         return (pad_int padding hour));
        (attempt (string "min:")
         >> padding
         >>= fun padding -> return (pad_int padding minute));
        (attempt (string "sec:")
         >> padding
         >>= fun padding -> return (pad_int padding second));
        attempt (string "ns") >> return (string_of_int ns);
        (attempt (string "sec-frac:")
         >> nat_zero
         >>= fun precision ->
         if precision = 0 then fail "Precision cannot be 0"
         else
           let ns = float_of_int ns in
           let precision' = float_of_int precision in
           let frac =
             ns *. (10. ** precision') /. Span.ns_count_in_s_float
             |> CCFloat.round
             |> int_of_float
           in
           return (Printf.sprintf "%0*d" precision frac));
        (attempt (string "tzoff-sign")
         >>= fun _ ->
         match single_offset with
         | None -> raise (Date_time_cannot_deduce_offset_from_utc date_time)
         | Some offset -> (
             match offset.sign with `Pos -> return "+" | `Neg -> return "-"));
        (attempt (string "tzoff-hour:")
         >> padding
         >>= fun padding ->
         match single_offset with
         | None -> raise (Date_time_cannot_deduce_offset_from_utc date_time)
         | Some offset -> return (pad_int padding Span.For_human'.(offset.hours))
        );
        (attempt (string "tzoff-min:")
         >> padding
         >>= fun padding ->
         match single_offset with
         | None -> raise (Date_time_cannot_deduce_offset_from_utc date_time)
         | Some offset ->
           return (pad_int padding Span.For_human'.(offset.minutes)));
        (attempt (string "tzoff-sec:")
         >> padding
         >>= fun padding ->
         match single_offset with
         | None -> raise (Date_time_cannot_deduce_offset_from_utc date_time)
         | Some offset ->
           return (pad_int padding Span.For_human'.(offset.seconds)));
        (* string "unix"
         * >> return (Int64.to_string (Time.Date_time'.to_timestamp date_time)); *)
      ]
end

let default_date_time_format_string =
  "{year} {mon:Xxx} {day:0X} {hour:0X}:{min:0X}:{sec:0X} \
   {tzoff-sign}{tzoff-hour:0X}:{tzoff-min:0X}:{tzoff-sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {sday:0X} {shour:0X}:{smin:0X}:{ssec:0X} \
   {stzoff-sign}{stzoff-hour:0X}:{stzoff-min:0X}:{stzoff-sec:0X}, {eyear} \
   {emon:Xxx} {eday:0X} {ehour:0X}:{emin:0X}:{esec:0X} \
   {etzoff-sign}{etzoff-hour:0X}:{etzoff-min:0X}:{etzoff-sec:0X})"

exception Invalid_format_string of string

let invalid_format_string s = raise (Invalid_format_string s)

let pp_date_time ?(format : string = default_date_time_format_string) ()
    (formatter : Format.formatter) (x : Date_time.t) : unit =
  let open MParser in
  let open Parser_components in
  let single formatter (date_time : Date_time.t) : (unit, unit) t =
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
  let p formatter (date_time : Date_time.t) : (unit, unit) t =
    many (single formatter date_time) >> return ()
  in
  match
    result_of_mparser_result @@ parse_string (p formatter x << eof) format ()
  with
  | Error msg -> invalid_format_string msg
  | Ok () -> ()

let string_of_date_time ?format (x : Date_time.t) : string option =
  try Some (Fmt.str "%a" (pp_date_time ?format ()) x)
  with Date_time_cannot_deduce_offset_from_utc _ -> None

let pp_timestamp ?(display_using_tz = Time_zone.utc) ?format () formatter time =
  match Date_time.of_timestamp ~tz_of_date_time:display_using_tz time with
  | None -> invalid_arg "Invalid unix second"
  | Some dt -> Fmt.pf formatter "%a" (pp_date_time ?format ()) dt

let string_of_timestamp ?display_using_tz ?format (time : Span.t) : string =
  Fmt.str "%a" (pp_timestamp ?display_using_tz ?format ()) time

let pp_interval ?(display_using_tz = Time_zone.utc)
    ?(format = default_interval_format_string) () formatter
    ((s, e) : Date_time.interval) : unit =
  let open MParser in
  let open Parser_components in
  let single (start_date_time : Date_time.t) (end_date_time : Date_time.t) :
    (unit, unit) t =
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
  let p (start_date_time : Date_time.t) (end_date_time : Date_time.t) :
    (unit, unit) t =
    many (single start_date_time end_date_time) >> return ()
  in
  match Date_time.of_timestamp ~tz_of_date_time:display_using_tz s with
  | None -> invalid_arg "Invalid start unix time"
  | Some s -> (
      match Date_time.of_timestamp ~tz_of_date_time:display_using_tz e with
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

let string_of_interval ?display_using_tz ?format (interval : Date_time.interval)
  : string =
  Fmt.str "%a" (pp_interval ?display_using_tz ?format ()) interval

let pp_intervals ?display_using_tz ?format ?(sep = Fmt.cut) () formatter
    intervals =
  Fmt.seq ~sep (pp_interval ?display_using_tz ?format ()) formatter intervals

let pp_span formatter ({ s; ns } : Span.t) : unit =
  Fmt.pf formatter "%Ld s + %d ns" s ns

let string_of_span (x : Span.t) : string = Fmt.str "%a" pp_span x

let pp_span_for_human formatter (x : Span.t) : unit =
  let (Span.For_human'.{ days; hours; minutes; seconds } : Span.For_human'.view)
    =
    Span.For_human'.view x
  in
  if days > 0 then
    Fmt.pf formatter "%d days %d hours %d mins %d secs" days hours minutes
      seconds
  else if hours > 0 then
    Fmt.pf formatter "%d hours %d mins %d secs" hours minutes seconds
  else if minutes > 0 then Fmt.pf formatter "%d mins %d secs" minutes seconds
  else Fmt.pf formatter "%d secs" seconds

let string_of_span_for_human (x : Span.t) : string =
  Fmt.str "%a" pp_span_for_human x

let wrap_to_sexp_into_pp_sexp (f : 'a -> CCSexp.t) :
  Format.formatter -> 'a -> unit =
  fun formatter x -> CCSexp.pp formatter (f x)
