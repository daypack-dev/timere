open Date_time_utils

let frac_s_1_divisor = Span.ns_count_in_s / 10

let frac_s_2_divisor = frac_s_1_divisor / 10

let frac_s_3_divisor = frac_s_2_divisor / 10

let frac_s_4_divisor = frac_s_3_divisor / 10

let frac_s_5_divisor = frac_s_4_divisor / 10

let frac_s_6_divisor = frac_s_5_divisor / 10

let frac_s_7_divisor = frac_s_6_divisor / 10

let frac_s_8_divisor = frac_s_7_divisor / 10

let frac_s_9_divisor = frac_s_8_divisor / 10

let get_divisor frac_s =
  match frac_s with
  | 1 -> frac_s_1_divisor
  | 2 -> frac_s_2_divisor
  | 3 -> frac_s_3_divisor
  | 4 -> frac_s_4_divisor
  | 5 -> frac_s_5_divisor
  | 6 -> frac_s_6_divisor
  | 7 -> frac_s_7_divisor
  | 8 -> frac_s_8_divisor
  | 9 -> frac_s_9_divisor
  | _ -> failwith "Unexpected case"

let deduce_smallest_lossless_frac_s ~ns =
  if ns = 0 then 0
  else if ns mod frac_s_1_divisor = 0 then 1
  else if ns mod frac_s_2_divisor = 0 then 2
  else if ns mod frac_s_3_divisor = 0 then 3
  else if ns mod frac_s_4_divisor = 0 then 4
  else if ns mod frac_s_5_divisor = 0 then 5
  else if ns mod frac_s_6_divisor = 0 then 6
  else if ns mod frac_s_7_divisor = 0 then 7
  else if ns mod frac_s_8_divisor = 0 then 8
  else 9

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

let string_of_s_frac ~sep ~frac_s ~ns =
  let ns = ns mod Span.ns_count_in_s in
  assert (0 <= frac_s && frac_s <= 9);
  if frac_s = 0 then ""
  else
    let divisor = get_divisor frac_s in
    Printf.sprintf "%c%0*d" sep frac_s (ns / divisor)

module Format_string_parsers = struct
  open Angstrom
  open Parser_components

  let case : case t =
    (char 'x' *> return Lower) <|> (char 'X' *> return Upper)

  let size_and_casing : size_and_casing t =
    case
    >>= fun c1 ->
    case
    >>= fun c2 ->
    (char '*' *> return (Full (c1, c2)))
    <|> (case >>= fun c3 -> return (Abbreviated (c1, c2, c3)))

  let padding : char option t =
    (satisfy (fun _ -> true)
     >>= fun padding -> char 'X' *> return (Some padding))
    <|> (char 'X' *> return None)

  let date_time_inner (date_time : Date_time.t) : string t =
    let single_offset =
      match date_time.offset_from_utc with
      | `Single offset -> Some (Span.For_human'.view offset)
      | `Ambiguous _ -> None
    in
    let Date.Ymd'.{ year; month; day } = Date_time.ymd_date date_time in
    let weekday = Date_time.weekday date_time in
    let Time.{ hour; minute; second; ns } = Date_time.time_view date_time in
    let smallest_lossless_frac_s = deduce_smallest_lossless_frac_s ~ns in
    choice
      [
        string "year" *> commit *> return (Printf.sprintf "%04d" year);
        string "mon:"
        *> commit
        *> (size_and_casing
            >>= (fun x ->
                return
                  (map_string_to_size_and_casing x
                     (Misc_utils.option_get_exn_or "Expected valid month"
                      @@ full_string_of_month month)))
                <|> (padding >>= fun padding -> return (pad_int padding month)));
        (string "day:"
         *> commit
         *> padding
         >>= fun padding -> return (pad_int padding day));
        (string "wday:"
         *> commit
         *> size_and_casing
         >>= fun x ->
         return
           (map_string_to_size_and_casing x (full_string_of_weekday weekday)));
        (string "hour:"
         *> commit
         *> padding
         >>= fun padding -> return (pad_int padding hour));
        (string "12hour:"
         *> commit
         *> padding
         >>= fun padding ->
         let hour =
           if hour = 0 then 12
           else if hour > 12 then hour - 12 else hour
         in
         return (pad_int padding hour));
        (string "min:"
         *> commit
         *> padding
         >>= fun padding -> return (pad_int padding minute));
        (string "sec:"
         *> commit
         *> padding
         >>= fun padding -> return (pad_int padding second));
        (string "ns" *> commit *> return (string_of_int ns));
        (string "sec-frac:"
         *> commit
         *> any_char
         >>= fun sep ->
         option smallest_lossless_frac_s nat_zero
         >>= fun frac_s ->
         if frac_s > 9 then
           fail "Number of digits after decimal separator cannot be > 9"
         else return (string_of_s_frac ~sep ~frac_s ~ns));
        (string "tzoff-sign"
         *> commit
         >>= fun _ ->
         match single_offset with
         | None -> raise (Date_time_cannot_deduce_offset_from_utc date_time)
         | Some offset -> (
             match offset.sign with `Pos -> return "+" | `Neg -> return "-"));
        (string "tzoff-hour:"
         *> commit
         *> padding
         >>= fun padding ->
         match single_offset with
         | None -> raise (Date_time_cannot_deduce_offset_from_utc date_time)
         | Some offset -> return (pad_int padding Span.For_human'.(offset.hours))
        );
        (string "tzoff-min:"
         *> commit
         *> padding
         >>= fun padding ->
         match single_offset with
         | None -> raise (Date_time_cannot_deduce_offset_from_utc date_time)
         | Some offset ->
           return (pad_int padding Span.For_human'.(offset.minutes)));
        (string "tzoff-sec:"
         *> commit
         *> padding
         >>= fun padding ->
         match single_offset with
         | None -> raise (Date_time_cannot_deduce_offset_from_utc date_time)
         | Some offset ->
           return (pad_int padding Span.For_human'.(offset.seconds)));
        (* string "unix"
         * >> return (Int64.to_string (Time.Date_time'.to_timestamp date_time)); *)
      ]

  let span_for_human_inner (view : Span.For_human'.view) : string t =
    let smallest_lossless_frac_s =
      deduce_smallest_lossless_frac_s ~ns:view.ns
    in
    let string_of_number_and_unit ~empty_on_zero ~padding (x : int)
        (unit_str : string) =
      if empty_on_zero && x = 0 then ""
      else Printf.sprintf "%s%s" (pad_int padding x) unit_str
    in
    let single ~empty_on_zero ~handle_padding ~name ~number =
      (string name *> if empty_on_zero then string "-nz:" else string ":")
      *> commit
      *> (if handle_padding then padding else return None)
      >>= fun padding ->
      non_curly_bracket_string
      >>= fun unit_str ->
      return (string_of_number_and_unit ~empty_on_zero ~padding number unit_str)
    in
    let sec_frac ~empty_on_zero ~ns =
      (string "sec-frac" *> commit
       *> if empty_on_zero then string "-nz:" else string ":")
      *> any_char
      >>= fun sep ->
      option smallest_lossless_frac_s nat_zero
      >>= fun frac_s ->
      if frac_s > 9 then
        fail "Number of digits after decimal separator cannot be > 9"
      else
        char 'X'
        *> non_curly_bracket_string
        >>= fun unit_str ->
        if empty_on_zero && ns = 0 then return ""
        else
          return
            (Printf.sprintf "%s%s" (string_of_s_frac ~sep ~frac_s ~ns) unit_str)
    in
    choice
      (
        let large_units =
          [
            (false, "days", view.days);
            (true, "hours", view.hours);
            (true, "mins", view.minutes);
            (true, "secs", view.seconds);
            (true, "ns", view.ns);
          ]
          |> List.map (fun (handle_padding, name, number) ->
              [
                single ~empty_on_zero:true ~handle_padding ~name ~number;
                single ~empty_on_zero:false ~handle_padding ~name ~number;
              ]
            )
          |> List.flatten
        in
        large_units
        @
        [
          sec_frac ~empty_on_zero:false ~ns:view.ns;
          sec_frac ~empty_on_zero:true ~ns:view.ns;
        ]
      )
end

let default_date_time_format_string =
  "{year} {mon:Xxx} {day:0X} {hour:0X}:{min:0X}:{sec:0X}{sec-frac:.} \
   {tzoff-sign}{tzoff-hour:0X}:{tzoff-min:0X}:{tzoff-sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {sday:0X} {shour:0X}:{smin:0X}:{ssec:0X}{ssec-frac:.} \
   {stzoff-sign}{stzoff-hour:0X}:{stzoff-min:0X}:{stzoff-sec:0X}, {eyear} \
   {emon:Xxx} {eday:0X} {ehour:0X}:{emin:0X}:{esec:0X}{esec-frac:.} \
   {etzoff-sign}{etzoff-hour:0X}:{etzoff-min:0X}:{etzoff-sec:0X})"

let default_span_for_human_format_string =
  "{days-nz: days }{hours-nz:X hours }{mins-nz:X mins }{secs:X}{sec-frac:.X} \
   secs"

exception Invalid_format_string of string

let invalid_format_string s = raise (Invalid_format_string s)

let pp_date_time ?(format : string = default_date_time_format_string) ()
    (formatter : Format.formatter) (x : Date_time.t) : unit =
  let open Angstrom in
  let single formatter (date_time : Date_time.t) : unit t =
    choice
      [
        (string "{{" >>| fun _ -> Format.fprintf formatter "{");
        (char '{'
         *> commit
         *> (Format_string_parsers.date_time_inner date_time <* char '}')
         >>| fun s -> Format.fprintf formatter "%s" s);
        (take_while1 (function '{' -> false | _ -> true)
         >>| fun s -> Format.fprintf formatter "%s" s);
      ]
  in
  let p formatter (date_time : Date_time.t) : unit t =
    many (single formatter date_time) *> return ()
  in
  match
    parse_string ~consume:All (p formatter x) format
  with
  | Error msg -> invalid_format_string msg
  | Ok () -> ()

let string_of_date_time ?format (x : Date_time.t) : string =
  Format.asprintf "%a" (pp_date_time ?format ()) x

let pp_timestamp ?(display_using_tz = Time_zone.utc) ?format () formatter time =
  match Date_time.of_timestamp ~tz_of_date_time:display_using_tz time with
  | None -> invalid_arg "Invalid unix second"
  | Some dt -> Format.fprintf formatter "%a" (pp_date_time ?format ()) dt

let string_of_timestamp ?display_using_tz ?format (time : Span.t) : string =
  Format.asprintf "%a" (pp_timestamp ?display_using_tz ?format ()) time

let pp_interval ?(display_using_tz = Time_zone.utc)
    ?(format = default_interval_format_string) () formatter
    ((s, e) : Date_time.interval) : unit =
  let open Angstrom in
  let single (start_date_time : Date_time.t) (end_date_time : Date_time.t)
    : unit t =
    choice
      [
        (string "{{" >>| fun _ -> Format.fprintf formatter "{");
        (char '{'
         *> ((char 's' *> return start_date_time)
             <|> (char 'e' *> return end_date_time))
         >>= fun date_time ->
         Format_string_parsers.date_time_inner date_time
         <* char '}'
         >>| fun s -> Format.fprintf formatter "%s" s);
        (take_while1 (function '{' -> false | _ -> true)
         >>| fun s -> Format.fprintf formatter "%s" s);
      ]
  in
  let p (start_date_time : Date_time.t) (end_date_time : Date_time.t)
    : unit t =
    many (single start_date_time end_date_time) *> return ()
  in
  match Date_time.of_timestamp ~tz_of_date_time:display_using_tz s with
  | None -> invalid_arg "Invalid start unix time"
  | Some s -> (
      match Date_time.of_timestamp ~tz_of_date_time:display_using_tz e with
      | None -> invalid_arg "Invalid end unix time"
      | Some e -> (
          match
            parse_string
              ~consume:All
              (p s e
               >>= fun s ->
               pos
               >>= fun pos ->
               ((end_of_input *> return s)
                <|> fail
                  (Printf.sprintf "Expected EOI, pos: %d"
                     pos)))
              format
          with
          | Error msg -> invalid_format_string msg
          | Ok () -> ()))

let string_of_interval ?display_using_tz ?format (interval : Date_time.interval)
  : string =
  Format.asprintf "%a" (pp_interval ?display_using_tz ?format ()) interval

let pp_intervals ?display_using_tz ?format ?sep () formatter
    intervals =
  Seq_utils_.pp ?sep (pp_interval ?display_using_tz ?format ()) formatter intervals

let pp_span formatter (x : Span.t) : unit =
  let s, ns = Span.to_s_ns x in
  Format.fprintf formatter "%Ld s + %d ns" s ns

let string_of_span (x : Span.t) : string = Format.asprintf "%a" pp_span x

let pp_span_for_human ?(format : string = default_span_for_human_format_string)
    () formatter (x : Span.t) : unit =
  let open Angstrom in
  let single formatter (view : Span.For_human'.view) : unit t =
    choice
      [
        (string "{{" >>| fun _ -> Format.fprintf formatter "{");
        (char '{'
         *> (Format_string_parsers.span_for_human_inner view <* char '}')
         >>| fun s -> Format.fprintf formatter "%s" s);
        (take_while1 (function '{' -> false | _ -> true)
         >>| fun s -> Format.fprintf formatter "%s" s);
      ]
  in
  let p formatter (view : Span.For_human'.view) : unit t =
    many (single formatter view) *> return ()
  in
  match
    parse_string ~consume:All (p formatter (Span.For_human'.view x)) format
  with
  | Error msg -> invalid_format_string msg
  | Ok () -> ()

let string_of_span_for_human ?format (x : Span.t) : string =
  Format.asprintf "%a" (pp_span_for_human ?format ()) x
