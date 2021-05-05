include Date_time_components
include Time_ast
include Time
include Infix
module Time_zone = Time_zone

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

  let of_sexp = Of_sexp.(wrap_of_sexp span_of_sexp)

  let of_sexp_string = Of_sexp.(wrap_of_sexp_into_of_sexp_string span_of_sexp)

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

module Date_time = struct
  include Date_time_components
  include Time.Date_time'

  type nonrec 'a local_result = 'a local_result

  let to_string = Printers.string_of_date_time

  exception
    Date_time_cannot_deduce_tz_offset_s = Printers
                                          .Date_time_cannot_deduce_tz_offset_s

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

  let of_sexp = Of_sexp.(wrap_of_sexp date_time_of_sexp)

  let of_sexp_string =
    Of_sexp.(wrap_of_sexp_into_of_sexp_string date_time_of_sexp)

  let pp_sexp = Printers.wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_date_time

  let min_of_local_result = min_of_local_result

  let max_of_local_result = max_of_local_result
end

module Week_date_time = struct
  include Time.Week_date_time'
end

module Hms = struct
  include Hms'

  let pp = Printers.pp_hms

  let to_string = Printers.string_of_hms
end

module Interval = struct
  include Interval'

  let pp = Printers.pp_interval

  let to_string = Printers.string_of_interval
end

type 'a range = 'a Range.range

module Points = Points

type points = Points.t

let resolve = Resolver.resolve

let pp_intervals = Printers.pp_intervals

let to_sexp = To_sexp.to_sexp

let to_sexp_string = To_sexp.to_sexp_string

let of_sexp = Of_sexp.(wrap_of_sexp of_sexp)

let of_sexp_string = Of_sexp.of_sexp_string

let pp_sexp = Printers.pp_sexp

module Utils = struct
  let ptime_span_of_span = Ptime_utils.ptime_span_of_span

  let ptime_of_timestamp = Ptime_utils.ptime_of_timestamp

  let span_of_ptime_span = Ptime_utils.span_of_ptime_span

  let timestamp_of_ptime = Ptime_utils.timestamp_of_ptime

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

  let flatten_month_ranges (months : int range Seq.t) : int Seq.t option =
    try Some (Month_ranges.Flatten.flatten months)
    with Range.Range_is_invalid -> None

  let flatten_month_range_list (months : int range list) : int list option =
    try Some (Month_ranges.Flatten.flatten_list months)
    with Range.Range_is_invalid -> None

  let flatten_month_day_ranges (month_days : int range Seq.t) : int Seq.t option
    =
    try Some (Month_day_ranges.Flatten.flatten month_days)
    with Range.Range_is_invalid -> None

  let flatten_month_day_range_list (month_days : int range list) :
    int list option =
    try Some (Month_day_ranges.Flatten.flatten_list month_days)
    with Range.Range_is_invalid -> None

  let flatten_weekday_ranges (weekdays : weekday range Seq.t) :
    weekday Seq.t option =
    try Some (Weekday_ranges.Flatten.flatten weekdays)
    with Range.Range_is_invalid -> None

  let flatten_weekday_range_list (weekdays : weekday range list) :
    weekday list option =
    try Some (Weekday_ranges.Flatten.flatten_list weekdays)
    with Range.Range_is_invalid -> None

  let weekday_of_tm_int = weekday_of_tm_int

  let tm_int_of_weekday = tm_int_of_weekday

  let get_local_tz_for_arg = Time_zone_utils.get_local_tz_for_arg
end
