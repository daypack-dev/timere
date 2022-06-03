let pp_ymd_date = RFC3339.pp_date

let pp_ym formatter (x : Ym.t) =
  let year, month = Ym.year_month x in
  Format.fprintf formatter "%04d-%02d" year month

let pp_iso_week formatter (x : ISO_week.t) =
  let year, week = ISO_week.year_week x in
  Format.fprintf formatter "%04d-W%02d" year week

let pp_iso_week_date formatter (x : Date.t) =
  let Date.ISO_week_date'.{ year; week; weekday } =
    Date.ISO_week_date'.view x
  in
  Format.fprintf formatter "%04d-W%02d-%d" year week
    (Date_time_utils.iso_int_of_weekday weekday)

let pp_iso_ord formatter (x : Date.t) =
  let Date.ISO_ord'.{ year; day_of_year } = Date.ISO_ord'.view x in
  Format.fprintf formatter "%04d-%03d" year day_of_year

let pp_iso_week_date_time ?frac_s () formatter (x : Date_time.t) =
  RFC3339.pp_date_time' ?frac_s pp_iso_week_date () formatter x

let pp_iso_ord_date_time ?frac_s () formatter (x : Date_time.t) =
  RFC3339.pp_date_time' ?frac_s pp_iso_ord () formatter x

let str_of_iso_week_date_time ?frac_s dt : string =
  Format.asprintf "%a" (pp_iso_week_date_time ?frac_s ()) dt

let str_of_iso_ord_date_time ?frac_s dt : string =
  Format.asprintf "%a" (pp_iso_ord_date_time ?frac_s ()) dt
