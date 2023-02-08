let pp_date formatter (date : Date.t) : unit =
  let wday = Date.weekday date in
  let Date.Ymd'.{ year; month; day } = Date.Ymd'.view date in
  Format.fprintf formatter "%s, %02d %s %04d"
    (Date_time_utils.abbr_string_of_weekday wday)
    day
    (Option.get @@ Date_time_utils.abbr_string_of_month month)
    year

let pp_time formatter (time : Time.t) : unit =
  let { Time.hour; minute; second; _ } = Time.view time in
  Format.fprintf formatter "%02d:%02d:%02d" hour minute second

let pp_timestamp formatter (x : Span.t) =
  match Date_time.of_timestamp ~tz_of_date_time:Time_zone.utc x with
  | None -> invalid_arg "Invalid timestamp"
  | Some dt ->
    Format.fprintf formatter "%a %a"
      pp_date (Date_time.date dt)
      pp_time (Date_time.time dt)

let pp_date_time formatter (dt : Date_time.t) =
  match Date_time.to_timestamp dt with
  | `Ambiguous _ -> raise (Printers.Date_time_cannot_deduce_offset_from_utc dt)
  | `Single x -> pp_timestamp formatter x

let of_date_time (dt : Date_time.t) : string =
  Format.asprintf "%a" pp_date_time dt

let of_timestamp (dt : Span.t) : string =
  Format.asprintf "%a" pp_timestamp dt
