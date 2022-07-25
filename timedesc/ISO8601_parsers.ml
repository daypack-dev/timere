let ym_p : Ym.t Angstrom.t =
  let open Angstrom in
  let open Parser_components in
  nat_zero
  >>= fun year ->
  char '-'
  *> max_two_digit_nat_zero
  >>= fun month ->
  match Ym.make ~year ~month with
  | Ok x -> return x
  | Error e ->
    fail
      (Printf.sprintf "Invalid date: %s"
         (Date_time.Ymd_date_time.string_of_error
            (e :> Date_time.Ymd_date_time.error)))

let ymd_p : Date.t Angstrom.t =
  let open Angstrom in
  let open Parser_components in
  ym_p
  >>= fun ym ->
  char '-'
  *> max_two_digit_nat_zero
  >>= fun day ->
  let year, month = Ym.year_month ym in
  match Date.Ymd'.make ~year ~month ~day with
  | Ok x -> return x
  | Error e ->
    fail
      (Printf.sprintf "Invalid date: %s"
         (Date_time.Ymd_date_time.string_of_error
            (e :> Date_time.Ymd_date_time.error)))

let iso_week_p : ISO_week.t Angstrom.t =
  let open Angstrom in
  let open Parser_components in
  nat_zero
  >>= fun year ->
  optional_char '-'
  *> char 'W'
  *> max_two_digit_nat_zero
  >>= fun week ->
  match ISO_week.make ~year ~week with
  | Ok x -> return x
  | Error e ->
    fail
      (Printf.sprintf "Invalid date: %s"
         (Date_time.ISO_week_date_time'.string_of_error
            (e :> Date_time.ISO_week_date_time'.error)))

let iso_week_date_p : Date.t Angstrom.t =
  let open Angstrom in
  let open Parser_components in
  let open Date_time_utils in
  iso_week_p
  >>= fun iso_week' ->
  char '-'
  *> one_digit_nat_zero
  >>= fun weekday ->
  match weekday_of_iso_int weekday with
  | None -> fail "Invalid weekday"
  | Some weekday -> (
      let year, week = ISO_week.year_week iso_week' in
      match Date.ISO_week_date'.make ~year ~week ~weekday with
      | Ok x -> return x
      | Error e ->
        fail
          (Printf.sprintf "Invalid date: %s"
             (Date_time.ISO_week_date_time'.string_of_error
                (e :> Date_time.ISO_week_date_time'.error))))

let iso_ord_p : Date.t Angstrom.t =
  let open Angstrom in
  let open Parser_components in
  nat_zero
  >>= fun year ->
  char '-'
  *> nat_zero
  >>= fun day_of_year ->
  match Date.ISO_ord'.make ~year ~day_of_year with
  | Ok x -> return x
  | Error e ->
    fail
      (Printf.sprintf "Invalid date: %s"
         (Date_time.ISO_ord_date_time'.string_of_error
            (e :> Date_time.ISO_ord_date_time'.error)))

let date_p : Date.t Angstrom.t =
  let open Angstrom in
  choice [ ymd_p; iso_week_date_p; iso_ord_p ]

let hm_p : (int * int) Angstrom.t =
  let open Angstrom in
  let open Parser_components in
  choice
    [
        (two_digit_nat_zero
         >>= fun hour ->
         optional_char ':'
         *> two_digit_nat_zero
         >>= fun minute -> return (hour, minute));
      (two_digit_nat_zero >>= fun hour -> return (hour, 0));
    ]

let hms_p =
  let open Angstrom in
  let open Parser_components in
  choice
    [
        (two_digit_nat_zero
         >>= fun hour ->
         optional_char ':'
         *> two_digit_nat_zero
         >>= fun minute ->
         optional_char ':'
         *> two_digit_nat_zero
         >>= fun second ->
         choice [ char '.'; char ',' ]
         *> num_string
         >>= fun s ->
         let s = if String.length s > 9 then String.sub s 0 9 else s in
         let len = String.length s in
         if len = 9 then return (hour, minute, second, int_of_string s)
         else
           let ns = int_of_string s * Printers.get_divisor len in
           return (hour, minute, second, ns));
        (two_digit_nat_zero
         >>= fun hour ->
         optional_char ':'
         *> two_digit_nat_zero
         >>= fun minute ->
         optional_char ':'
         *> two_digit_nat_zero
         >>= fun second -> return (hour, minute, second, 0));
      (hm_p >>| fun (hour, minute) -> (hour, minute, 0, 0));
    ]

let time_p : Time.t Angstrom.t =
  let open Angstrom in
  hms_p
  >>= fun (hour, minute, second, ns) ->
  match Time.make ~hour ~minute ~second ~ns () with
  | Ok x -> return x
  | Error e ->
    fail
      (Printf.sprintf "Invalid time: %s"
         (Date_time.Ymd_date_time.string_of_error
            (e :> Date_time.Ymd_date_time.error)))

let offset_p : Span.t Angstrom.t =
  let open Angstrom in
  (char 'Z' *> return Span.zero)
      <|> ((char '+' *> return `Pos)
           <|>
           (char '-' *> return `Neg)
           >>= fun sign ->
           hms_p
           >>| fun (hour, minute, second, _ns) ->
           Span.For_human'.make_exn ~sign ~hours:hour ~minutes:minute ~seconds:second ())

type maybe_zoneless =
  [ `Zoned of Date_time.t
  | `Zoneless of Date_time.Zoneless'.zoneless
  ]

let maybe_zoneless_of_str' date_p s : (maybe_zoneless, string) result =
  let open Angstrom in
  let open Parser_components in
  let p =
    date_p
    >>= fun date ->
    any_char
    *> time_p
    >>= fun time ->
    (offset_p <* spaces <* end_of_input)
    >>= (fun offset ->
        match
          Date_time.Zoneless'.to_zoned_unambiguous ~offset_from_utc:offset
            (Date_time.Zoneless'.make date time)
        with
        | Error e ->
          fail
            (Printf.sprintf "Invalid date time: %s"
               (Date_time.Ymd_date_time.string_of_error
                  (e :> Date_time.Ymd_date_time.error)))
        | Ok x -> return (`Zoned x))
        <|> (spaces
             *> end_of_input
             *> return (`Zoneless (Date_time.Zoneless'.make date time)))
  in
  parse_string ~consume:All (p <* spaces) s

let maybe_zoneless_of_str s : (maybe_zoneless, string) result =
  maybe_zoneless_of_str' date_p s

let iso_week_date_time_maybe_zoneless_of_str s : (maybe_zoneless, string) result
  =
  maybe_zoneless_of_str' iso_week_date_p s

let iso_ord_date_time_maybe_zoneless_of_str s : (maybe_zoneless, string) result
  =
  maybe_zoneless_of_str' iso_ord_p s

let zoneless_of_str s : (Date_time.Zoneless'.zoneless, string) result =
  match maybe_zoneless_of_str s with
  | Ok (`Zoneless x) -> Ok x
  | Ok (`Zoned _) -> Error "Extraneous offset from utc"
  | Error msg -> Error msg

let date_time_of_str' maybe_zoneless_of_str s : (Date_time.t, string) result =
  match maybe_zoneless_of_str s with
  | Ok (`Zoneless _) -> Error "Missing offset from utc"
  | Ok (`Zoned x) -> Ok x
  | Error msg -> Error msg

let date_time_of_str s = date_time_of_str' maybe_zoneless_of_str s

let iso_week_date_time_of_str s =
  date_time_of_str' iso_week_date_time_maybe_zoneless_of_str s

let iso_ord_date_time_of_str s =
  date_time_of_str' iso_ord_date_time_maybe_zoneless_of_str s

let of_str' p s =
  let open Angstrom in
  let open Parser_components in
  parse_string ~consume:All (p <* spaces) s

let ym_of_str s = of_str' ym_p s

let date_of_str s = of_str' date_p s

let ymd_of_str s = of_str' ymd_p s

let iso_week_date_of_str s = of_str' iso_week_date_p s

let iso_ord_of_str s = of_str' iso_ord_p s

let time_of_str s = of_str' time_p s

let timestamp_of_str s =
  match date_time_of_str s with
  | Ok dt -> Ok (Date_time.to_timestamp_single dt)
  | Error msg -> Error msg

let iso_week_of_str = of_str' iso_week_p
