let pp_date formatter (date : Date.t) =
  let Date.Ymd'.{ year; month; day } = Date.Ymd'.view date in
  Fmt.pf formatter "%04d-%02d-%02d" year month day

let pp_time ?frac_s () formatter (time : Time.t) =
  let { Time.hour; minute; second; ns } = Time.view time in
  let ns = ns mod Span.ns_count_in_s in
  let frac_s =
    match frac_s with
    | None -> Printers.deduce_smallest_lossless_frac_s ~ns
    | Some x -> x
  in
  if frac_s < 0 then invalid_arg "pp_time: frac_s cannot be < 0"
  else if frac_s > 9 then invalid_arg "pp_time: frac_s cannot be > 9"
  else
    let second = if Time.is_leap_second time then 60 else second in
    Fmt.pf formatter "%02d:%02d:%02d%s" hour minute second
      (Printers.string_of_s_frac ~sep:'.' ~frac_s ~ns)

let pp_date_time' ?frac_s pp_date () formatter (dt : Date_time.t) =
  match Date_time.offset_from_utc dt with
  | `Ambiguous _ -> raise (Printers.Date_time_cannot_deduce_offset_from_utc dt)
  | `Single offset ->
    let offset_view = Span.For_human'.view offset in
    let tz_off =
      if Span.(offset = zero) then "Z"
      else
        let sign = match offset_view.sign with `Pos -> '+' | `Neg -> '-' in
        Printf.sprintf "%c%02d:%02d:%02d" sign
          offset_view.hours
          offset_view.minutes
          offset_view.seconds
    in
    Fmt.pf formatter "%aT%a%s" pp_date (Date_time.date dt)
      (pp_time ?frac_s ()) (Date_time.time dt) tz_off

let pp_date_time ?frac_s () formatter (dt : Date_time.t) =
  pp_date_time' ?frac_s pp_date () formatter dt

let of_date_time ?frac_s (dt : Date_time.t) : string option =
  try Some (Fmt.str "%a" (pp_date_time ?frac_s ()) dt)
  with Printers.Date_time_cannot_deduce_offset_from_utc _ -> None

let of_time ?frac_s (time : Time.t) : string =
  Fmt.str "%a" (pp_time ?frac_s ()) time

let pp_timestamp ?frac_s () formatter (x : Span.t) =
  match Date_time.of_timestamp ~tz_of_date_time:Time_zone.utc x with
  | None -> invalid_arg "Invalid timestamp"
  | Some dt -> Fmt.pf formatter "%a" (pp_date_time ?frac_s ()) dt

let of_timestamp ?frac_s (x : Span.t) : string =
  Fmt.str "%a" (pp_timestamp ?frac_s ()) x
