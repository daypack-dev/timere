let pp_ymd_date = RFC3339.pp_date

let pp_ym formatter (x : Ym.t) =
  let year, month = Ym.year_month x in
  Fmt.pf formatter "%04d-%02d" year month

let pp_iso_week formatter (x : ISO_week.t) =
  let year, week = ISO_week.year_week x in
  Fmt.pf formatter "%04d-W%02d" year week

let pp_iso_week_date formatter (x : Date.t) =
  let Date.ISO_week_date'.{ year; week; weekday } =
    Date.ISO_week_date'.view x
  in
  Fmt.pf formatter "%04d-W%02d-%d" year week
    (Date_time_utils.iso_int_of_weekday weekday)

let pp_iso_ord_date formatter (x : Date.t) =
  let Date.ISO_ord_date'.{ year; day_of_year } = Date.ISO_ord_date'.view x in
  Fmt.pf formatter "%04d-%03d" year day_of_year
