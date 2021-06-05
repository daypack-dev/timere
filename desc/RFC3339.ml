let pp_date_time ?frac_s () formatter (dt : Date_time.t) =
  let { Time.hour; minute; second; ns } = Date_time.time_view dt in
  let ns = ns mod Span.ns_count_in_s in
  let frac_s =
    match frac_s with
    | None -> Printers.deduce_smallest_lossless_frac_s ~ns
    | Some x -> x
  in
  if frac_s < 0 then invalid_arg "pp_date_time: frac_s cannot be < 0"
  else if frac_s > 9 then invalid_arg "pp_date_time: frac_s cannot be > 9"
  else
    match Date_time.offset_from_utc dt with
    | `Ambiguous _ ->
        raise (Printers.Date_time_cannot_deduce_offset_from_utc dt)
    | `Single offset ->
        let offset_view = Span.For_human'.view offset in
        let tz_off =
          if Span.(offset = zero) then "Z"
          else
            let sign =
              match offset_view.sign with `Pos -> '+' | `Neg -> '-'
            in
            Printf.sprintf "%c%02d:%02d" sign offset_view.hours
              offset_view.minutes
        in
        let second = if Date_time.is_leap_second dt then 60 else second in
        let Date.Ymd_date.{ year; month; day } = Date_time.ymd_date dt in
        Fmt.pf formatter "%04d-%02d-%02dT%02d:%02d:%02d%s%s" year month day hour
          minute second
          (Printers.string_of_s_frac ~sep:'.' ~frac_s ~ns)
          tz_off

let of_date_time ?frac_s (dt : Date_time.t) : string option =
  try Some (Fmt.str "%a" (pp_date_time ?frac_s ()) dt)
  with Printers.Date_time_cannot_deduce_offset_from_utc _ -> None

let pp_timestamp ?frac_s () formatter (x : Span.t) =
  match Date_time.of_timestamp ~tz_of_date_time:Time_zone.utc x with
  | None -> invalid_arg "Invalid timestamp"
  | Some dt -> Fmt.pf formatter "%a" (pp_date_time ?frac_s ()) dt

let of_timestamp ?frac_s (x : Span.t) : string =
  Fmt.str "%a" (pp_timestamp ?frac_s ()) x
