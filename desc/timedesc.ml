include Date_time_components
include Date_time
module Time_zone = Time_zone
module Date = Date
module Time = Time

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

  let to_sexp = To_sexp.sexp_of_span

  let to_sexp_string x = CCSexp.to_string (To_sexp.sexp_of_span x)

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.span_of_sexp

  let of_sexp_string =
    Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.span_of_sexp

  let pp_sexp = Printers.wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_span
end

module Timestamp = struct
  let min_val = timestamp_min

  let max_val = timestamp_max

  let now = timestamp_now

  let pp = Printers.pp_timestamp

  let to_string = Printers.string_of_timestamp

  let pp_rfc3339 = RFC3339.pp_timestamp

  let pp_rfc3339_milli = RFC3339.pp_timestamp ~frac_s:3 ()

  let pp_rfc3339_micro = RFC3339.pp_timestamp ~frac_s:6 ()

  let pp_rfc3339_nano = RFC3339.pp_timestamp ~frac_s:9 ()

  let to_rfc3339 = RFC3339.of_timestamp

  let to_rfc3339_milli = RFC3339.of_timestamp ~frac_s:3

  let to_rfc3339_micro = RFC3339.of_timestamp ~frac_s:6

  let to_rfc3339_nano = RFC3339.of_timestamp ~frac_s:9

  let of_iso8601 = ISO8601.to_timestamp
end

let to_string = Printers.string_of_date_time

exception
  Date_time_cannot_deduce_offset_from_utc = Printers
                                        .Date_time_cannot_deduce_offset_from_utc

let pp = Printers.pp_date_time

let pp_rfc3339 = RFC3339.pp_date_time

let pp_rfc3339_milli = RFC3339.pp_date_time ~frac_s:3 ()

let pp_rfc3339_micro = RFC3339.pp_date_time ~frac_s:6 ()

let pp_rfc3339_nano = RFC3339.pp_date_time ~frac_s:9 ()

let to_rfc3339 = RFC3339.of_date_time

let to_rfc3339_milli = RFC3339.of_date_time ~frac_s:3

let to_rfc3339_micro = RFC3339.of_date_time ~frac_s:6

let to_rfc3339_nano = RFC3339.of_date_time ~frac_s:9

let of_iso8601 = ISO8601.to_date_time

let to_sexp = To_sexp.sexp_of_date_time

let to_sexp_string x = CCSexp.to_string (To_sexp.sexp_of_date_time x)

let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.date_time_of_sexp

let of_sexp_string =
  Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.date_time_of_sexp

let pp_sexp = Printers.wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_date_time

let min_of_local_result = min_of_local_result

let max_of_local_result = max_of_local_result

include Ymd_date_time

module Time_zone_info = struct
  include Time_zone_info

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.tz_info_of_sexp

  let to_sexp = To_sexp.sexp_of_tz_info
end

module Utils = struct
  let ptime_span_of_span = Ptime_utils.ptime_span_of_span

  let ptime_of_timestamp = Ptime_utils.ptime_of_timestamp

  let span_of_ptime_span = Ptime_utils.span_of_ptime_span

  let timestamp_of_ptime = Ptime_utils.timestamp_of_ptime

  let day_count_of_year = day_count_of_year

  let day_count_of_month = day_count_of_month

  let week_count_of_iso_week_year = week_count_of_iso_week_year

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

  let get_local_tz_for_arg = Time_zone_utils.get_local_tz_for_arg
end
