open Date_time_components

let of_date_time (dt : Time.Date_time'.t) : string =
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d%s" dt.year
    (human_int_of_month dt.month)
    dt.day dt.hour dt.minute dt.second
    (match dt.tz_info with
     | `Tz_only _ -> invalid_arg "Date time has no exact offset"
     | `Tz_offset_s_only x | `Tz_and_tz_offset_s (_, x) ->
       if x = 0 then "Z"
       else
         let sign = if x < 0 then '-' else '+' in
         let offset = Duration.of_seconds (Int64.of_int (abs x)) in
         Printf.sprintf "%c%02d:%02d" sign offset.hours offset.minutes)

let pp_date_time formatter dt = Format.fprintf formatter "%s" (of_date_time dt)

let of_timestamp (x : int64) : string =
  match Time.Date_time'.of_timestamp ~tz_of_date_time:Time_zone.utc x with
  | None -> invalid_arg "Invalid timestamp"
  | Some dt -> of_date_time dt

let pp_timestamp formatter x = Format.fprintf formatter "%s" (of_timestamp x)
