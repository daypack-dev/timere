open Date_time_components

let pp_date_time formatter (dt : Time.Date_time'.t) =
  match dt.tz_info with
  | `Tz_only _ -> raise (Printers.Date_time_cannot_deduce_tz_offset_s dt)
  | `Tz_offset_s_only x | `Tz_and_tz_offset_s (_, x) ->
    let tz_off =
      if x = 0 then "Z"
      else
        let sign = if x < 0 then '-' else '+' in
        let offset = Duration.make ~seconds:(abs x) () in
        Printf.sprintf "%c%02d:%02d" sign offset.hours offset.minutes
    in
    Fmt.pf formatter
     "%04d-%02d-%02dT%02d:%02d:%02d%s" dt.year
      (human_int_of_month dt.month)
      dt.day dt.hour dt.minute dt.second tz_off

let of_date_time (dt : Time.Date_time'.t) : string option =
  try
    Some (Fmt.str "%a" pp_date_time dt)
  with
  | Printers.Date_time_cannot_deduce_tz_offset_s _ -> None

let pp_timestamp formatter (x : Span.t) =
  match Time.Date_time'.of_timestamp ~tz_of_date_time:Time_zone.utc x with
  | None -> invalid_arg "Invalid timestamp"
  | Some dt -> Fmt.pf formatter "%a" pp_date_time dt

let of_timestamp (x : Span.t) : string =
  Fmt.str "%a" pp_timestamp x
