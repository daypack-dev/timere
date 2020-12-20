include Time
module Time_zone = Time_zone

module Date_time = struct
  include Date_time

  type 'a local_result = 'a Time_zone.local_result

  let sprintf = Printers.sprintf_date_time

  let pp = Printers.pp_date_time
end

module Duration = struct
  include Duration

  let sprint = Printers.sprint_duration

  let pp = Printers.pp_duration
end

type 'a range = 'a Range.range

type interval = Interval.t

module Infix = struct
  let ( & ) a b = Time.inter [ a; b ]

  let ( ||| ) a b = Time.union [ a; b ]

  let ( -- ) = Time.interval_inc

  let ( --^ ) = Time.interval_exc
end

let resolve = Resolver.resolve

let sprintf_timestamp = Printers.sprintf_timestamp

let pp_timestamp = Printers.pp_timestamp

let sprintf_interval = Printers.sprintf_interval

let pp_interval = Printers.pp_interval

let to_sexp = To_sexp.to_sexp

let pp_sexp = Printers.pp_sexp

let to_sexp_string = To_sexp.to_sexp_string

let of_sexp = Of_sexp.of_sexp

let of_sexp_string = Of_sexp.of_sexp_string

module Utils = struct
  let flatten_month_ranges (months : month range Seq.t) :
    (month Seq.t, unit) Result.t =
    try Ok (Month_ranges.Flatten.flatten months)
    with Range.Range_is_invalid -> Error ()

  let flatten_month_range_list (months : month range list) :
    (month list, unit) Result.t =
    try Ok (Month_ranges.Flatten.flatten_list months)
    with Range.Range_is_invalid -> Error ()

  let flatten_month_day_ranges (month_days : int range Seq.t) :
    (int Seq.t, unit) Result.t =
    try Ok (Month_day_ranges.Flatten.flatten month_days)
    with Range.Range_is_invalid -> Error ()

  let flatten_month_day_range_list (month_days : int range list) :
    (int list, unit) Result.t =
    try Ok (Month_day_ranges.Flatten.flatten_list month_days)
    with Range.Range_is_invalid -> Error ()

  let flatten_weekday_ranges (weekdays : weekday range Seq.t) :
    (weekday Seq.t, unit) Result.t =
    try Ok (Weekday_ranges.Flatten.flatten weekdays)
    with Range.Range_is_invalid -> Error ()

  let flatten_weekday_range_list (weekdays : weekday range list) :
    (weekday list, unit) Result.t =
    try Ok (Weekday_ranges.Flatten.flatten_list weekdays)
    with Range.Range_is_invalid -> Error ()

  let resolve_simple = Simple_resolver.resolve
end
