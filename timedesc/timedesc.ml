include Date_time_utils
include Date_time
module Time_zone = Time_zone

exception ISO8601_parse_exn of string

exception RFC9110_parse_exn of string

let frac_s_milli = 3

let frac_s_micro = 6

let frac_s_nano = 9

let of_iso8601_exn' of_iso8601 s =
  match of_iso8601 s with
  | Ok x -> x
  | Error msg -> raise (ISO8601_parse_exn msg)

let of_rfc9110_exn' of_rfc9110 s =
  match of_rfc9110 s with
  | Ok x -> x
  | Error msg -> raise (RFC9110_parse_exn msg)

let str_of_pp pp x = Format.asprintf "%a" pp x

module Ym = struct
  include Ym

  let pp_iso8601 = ISO8601_printers.pp_ym

  let to_iso8601 x = str_of_pp pp_iso8601 x

  let of_iso8601 = ISO8601_parsers.ym_of_str

  let of_iso8601_exn s = of_iso8601_exn' of_iso8601 s
end

module ISO_week = struct
  include ISO_week

  let pp_iso8601 = ISO8601_printers.pp_iso_week

  let to_iso8601 x = str_of_pp pp_iso8601 x

  let of_iso8601 = ISO8601_parsers.iso_week_of_str

  let of_iso8601_exn s = of_iso8601_exn' of_iso8601 s
end

module Date = struct
  include Date

  let pp_rfc3339 = RFC3339.pp_date

  let to_rfc3339 x = str_of_pp pp_rfc3339 x

  let of_iso8601 = ISO8601_parsers.date_of_str

  let of_iso8601_exn s = of_iso8601_exn' of_iso8601 s

  let pp_rfc9110 = RFC9110_printers.pp_date

  let to_rfc9110 x = str_of_pp pp_rfc9110 x

  let pp_http = pp_rfc9110

  let to_http = to_rfc9110

  module Ymd = struct
    include Ymd'

    let pp_iso8601 = ISO8601_printers.pp_ymd_date

    let to_iso8601 x = str_of_pp pp_iso8601 x

    let of_iso8601 = ISO8601_parsers.ymd_of_str

    let of_iso8601_exn = of_iso8601_exn' of_iso8601
  end

  module ISO_week_date = struct
    include ISO_week_date'

    let pp_iso8601 = ISO8601_printers.pp_iso_week_date

    let to_iso8601 x = str_of_pp pp_iso8601 x

    let of_iso8601 = ISO8601_parsers.iso_week_date_of_str

    let of_iso8601_exn = of_iso8601_exn' of_iso8601
  end

  module ISO_ord = struct
    include ISO_ord'

    let pp_iso8601 = ISO8601_printers.pp_iso_ord

    let to_iso8601 x = str_of_pp pp_iso8601 x

    let of_iso8601 = ISO8601_parsers.iso_ord_of_str

    let of_iso8601_exn = of_iso8601_exn' of_iso8601
  end
end

module Time = struct
  include Time

  let pp_rfc3339 = RFC3339.pp_time

  let pp_rfc3339_milli = pp_rfc3339 ~frac_s:frac_s_milli ()

  let pp_rfc3339_micro = pp_rfc3339 ~frac_s:frac_s_micro ()

  let pp_rfc3339_nano = pp_rfc3339 ~frac_s:frac_s_nano ()

  let to_rfc3339 = RFC3339.of_time

  let to_rfc3339_milli = to_rfc3339 ~frac_s:frac_s_milli

  let to_rfc3339_micro = to_rfc3339 ~frac_s:frac_s_micro

  let to_rfc3339_nano = to_rfc3339 ~frac_s:frac_s_nano

  let of_iso8601 = ISO8601_parsers.time_of_str

  let of_iso8601_exn s = of_iso8601_exn' of_iso8601 s
end

exception Invalid_format_string = Printers.Invalid_format_string

module Span = struct
  include Span

  module For_human = struct
    include For_human'

    let to_string = Printers.string_of_span_for_human

    let pp = Printers.pp_span_for_human
  end

  let to_string = Printers.string_of_span

  let pp = Printers.pp_span
end

module Timestamp = struct
  include Span

  let min_val = timestamp_min

  let max_val = timestamp_max

  let now = timestamp_now

  let pp = Printers.pp_timestamp

  let to_string = Printers.string_of_timestamp

  let pp_rfc3339 = RFC3339.pp_timestamp

  let pp_rfc3339_milli = pp_rfc3339 ~frac_s:frac_s_milli ()

  let pp_rfc3339_micro = pp_rfc3339 ~frac_s:frac_s_micro ()

  let pp_rfc3339_nano = pp_rfc3339 ~frac_s:frac_s_nano ()

  let pp_iso8601 = pp_rfc3339

  let pp_iso8601_milli = pp_rfc3339_milli

  let pp_iso8601_micro = pp_rfc3339_micro

  let pp_iso8601_nano = pp_rfc3339_nano

  let pp_rfc9110 = RFC9110_printers.pp_timestamp

  let to_rfc3339 = RFC3339.of_timestamp

  let pp_http = pp_rfc9110

  let to_rfc3339_milli = to_rfc3339 ~frac_s:frac_s_milli

  let to_rfc3339_micro = to_rfc3339 ~frac_s:frac_s_micro

  let to_rfc3339_nano = to_rfc3339 ~frac_s:frac_s_nano

  let to_iso8601 = to_rfc3339

  let to_iso8601_milli = to_rfc3339_milli

  let to_iso8601_micro = to_rfc3339_micro

  let to_iso8601_nano = to_rfc3339_nano

  let to_rfc9110 = RFC9110_printers.of_timestamp

  let to_http = to_rfc9110

  let of_iso8601 = ISO8601_parsers.timestamp_of_str

  let of_iso8601_exn = of_iso8601_exn' of_iso8601

  let of_rfc9110 = RFC9110_parsers.timestamp_of_str

  let of_rfc9110_exn = of_rfc9110_exn' of_rfc9110

  let of_http = of_rfc9110

  let of_http_exn = of_rfc9110_exn
end

let to_string = Printers.string_of_date_time

exception Date_time_cannot_deduce_offset_from_utc =
  Printers.Date_time_cannot_deduce_offset_from_utc

let pp = Printers.pp_date_time

let pp_rfc3339 = RFC3339.pp_date_time

let pp_rfc3339_milli = pp_rfc3339 ~frac_s:frac_s_milli ()

let pp_rfc3339_micro = pp_rfc3339 ~frac_s:frac_s_micro ()

let pp_rfc3339_nano = pp_rfc3339 ~frac_s:frac_s_nano ()

let pp_iso8601 = pp_rfc3339

let pp_iso8601_milli = pp_rfc3339_milli

let pp_iso8601_micro = pp_rfc3339_micro

let pp_iso8601_nano = pp_rfc3339_nano

let pp_rfc9110 = RFC9110_printers.pp_date_time

let to_rfc3339 = RFC3339.of_date_time

let pp_http = pp_rfc9110

let to_rfc3339_milli = to_rfc3339 ~frac_s:frac_s_milli

let to_rfc3339_micro = to_rfc3339 ~frac_s:frac_s_micro

let to_rfc3339_nano = to_rfc3339 ~frac_s:frac_s_nano

let to_iso8601 = to_rfc3339

let to_iso8601_milli = to_rfc3339_milli

let to_iso8601_micro = to_rfc3339_micro

let to_iso8601_nano = to_rfc3339_nano

let to_rfc9110 = RFC9110_printers.of_date_time

let to_http = to_rfc9110

let of_iso8601 = ISO8601_parsers.date_time_of_str

let of_iso8601_exn = of_iso8601_exn' of_iso8601

let of_rfc9110 = RFC9110_parsers.date_time_of_str

let of_rfc9110_exn = of_rfc9110_exn' of_rfc9110

let of_http = of_rfc9110

let of_http_exn = of_rfc9110_exn

let min_of_local_dt_result = min_of_local_dt_result

let max_of_local_dt_result = max_of_local_dt_result

include Ymd_date_time

module ISO_week_date_time = struct
  include ISO_week_date_time'

  let pp_iso8601 = ISO8601_printers.pp_iso_week_date_time

  let pp_iso8601_milli = pp_iso8601 ~frac_s:frac_s_milli ()

  let pp_iso8601_micro = pp_iso8601 ~frac_s:frac_s_micro ()

  let pp_iso8601_nano = pp_iso8601 ~frac_s:frac_s_nano ()

  let to_iso8601 = ISO8601_printers.str_of_iso_week_date_time

  let to_iso8601_milli = to_iso8601 ~frac_s:frac_s_milli

  let to_iso8601_micro = to_iso8601 ~frac_s:frac_s_micro

  let to_iso8601_nano = to_iso8601 ~frac_s:frac_s_nano

  let of_iso8601 = ISO8601_parsers.iso_week_date_time_of_str

  let of_iso8601_exn = of_iso8601_exn' of_iso8601
end

module ISO_ord_date_time = struct
  include ISO_ord_date_time'

  let pp_iso8601 = ISO8601_printers.pp_iso_ord_date_time

  let pp_iso8601_milli = pp_iso8601 ~frac_s:frac_s_milli ()

  let pp_iso8601_micro = pp_iso8601 ~frac_s:frac_s_micro ()

  let pp_iso8601_nano = pp_iso8601 ~frac_s:frac_s_nano ()

  let to_iso8601 = ISO8601_printers.str_of_iso_ord_date_time

  let to_iso8601_milli = to_iso8601 ~frac_s:frac_s_milli

  let to_iso8601_micro = to_iso8601 ~frac_s:frac_s_micro

  let to_iso8601_nano = to_iso8601 ~frac_s:frac_s_nano

  let of_iso8601 = ISO8601_parsers.iso_ord_date_time_of_str

  let of_iso8601_exn = of_iso8601_exn' of_iso8601
end

module Interval = struct
  type t = timestamp * timestamp

  let equal (x1, y1) (x2, y2) = Span.(x1 = x2 && y1 = y2)

  let lt (x1, y1) (x2, y2) =
    (* lexicographic order *)
    Span.(x1 < x2 || (x1 = x2 && y1 < y2))

  let le x y = lt x y || equal x y

  let gt x y = lt y x

  let ge x y = le y x

  let compare x y = if lt x y then -1 else if x = y then 0 else 1

  let pp = Printers.pp_interval

  let to_string = Printers.string_of_interval

  let pp_seq = Printers.pp_intervals
end

module Zoneless = struct
  include Zoneless'

  let of_iso8601 = ISO8601_parsers.zoneless_of_str

  let of_iso8601_exn = of_iso8601_exn' of_iso8601

  let maybe_zoneless_of_iso8601 = ISO8601_parsers.maybe_zoneless_of_str

  let maybe_zoneless_of_iso8601_exn = of_iso8601_exn' maybe_zoneless_of_iso8601
end

module Time_zone_info = Time_zone_info

module Utils = struct
  let ptime_span_of_span = Ptime_utils.ptime_span_of_span

  let ptime_of_timestamp = Ptime_utils.ptime_of_timestamp

  let span_of_ptime_span = Ptime_utils.span_of_ptime_span

  let timestamp_of_ptime = Ptime_utils.timestamp_of_ptime

  let day_count_of_year = day_count_of_year

  let day_count_of_month = day_count_of_month

  let week_count_of_iso_year = week_count_of_iso_year

  type month =
    [ `Jan
    | `Feb
    | `Mar
    | `Apr
    | `May
    | `Jun
    | `Jul
    | `Aug
    | `Sep
    | `Oct
    | `Nov
    | `Dec
    ]

  let human_int_of_month (x : month) =
    match x with
    | `Jan -> 1
    | `Feb -> 2
    | `Mar -> 3
    | `Apr -> 4
    | `May -> 5
    | `Jun -> 6
    | `Jul -> 7
    | `Aug -> 8
    | `Sep -> 9
    | `Oct -> 10
    | `Nov -> 11
    | `Dec -> 12

  let index_of_month (x : month) = pred (human_int_of_month x)

  let month_of_human_int (x : int) : month option =
    match x with
    | 1 -> Some `Jan
    | 2 -> Some `Feb
    | 3 -> Some `Mar
    | 4 -> Some `Apr
    | 5 -> Some `May
    | 6 -> Some `Jun
    | 7 -> Some `Jul
    | 8 -> Some `Aug
    | 9 -> Some `Sep
    | 10 -> Some `Oct
    | 11 -> Some `Nov
    | 12 -> Some `Dec
    | _ -> None

  let month_of_index (x : int) : month option = month_of_human_int (succ x)

  let weekday_of_tm_int = weekday_of_tm_int

  let tm_int_of_weekday = tm_int_of_weekday

  let weekday_of_iso_int = weekday_of_iso_int

  let iso_int_of_weekday = iso_int_of_weekday

  let get_local_tz_for_arg = Time_zone_utils.get_local_tz_for_arg

  let abbr_string_of_weekday = abbr_string_of_weekday

  let is_leap_year = is_leap_year

  let jd_of_ymd = jd_of_ymd

  let jd_of_ydoy = jd_of_ydoy

  let jd_of_date (x : Date.t) = x.jd

  let jd_of_unix_epoch = jd_of_unix_epoch

  let jd_span_of_unix_epoch = jd_span_of_unix_epoch

  let ymd_of_jd = ymd_of_jd

  let weekday_of_jd = weekday_of_jd

  let doy_of_ymd = doy_of_ymd

  let jd_of_iso_week_date = jd_of_iso_week_date

  let iso_week_date_of_jd = iso_week_date_of_jd
end
