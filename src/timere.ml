include Time

module Date_time = struct
  include Date_time

  let sprintf = Printer.sprintf_date_time

  let pp = Printer.pp_date_time
end

module Duration = struct
  include Duration

  let sprint = Printer.sprint_duration

  let pp = Printer.pp_duration
end

type 'a range = 'a Range.range

type interval = Interval.t

module Infix = struct
  let ( & ) = Time.inter

  let ( ||| ) = Time.union

  let ( -- ) = Time.interval_inc

  let ( --^ ) = Time.interval_exc

  let ( --* ) = Time.intervals_inc

  let ( --*^ ) = Time.intervals_exc
end

let resolve = Resolver.resolve

module Utils = struct
  let flatten_month_ranges (months : month range Seq.t) :
    (month Seq.t, unit) Result.t =
    try Ok (Month_ranges.Flatten.flatten months)
    with Range.Range_is_invalid -> Error ()

  let flatten_month_range_list (months : month range list) :
    (month list, unit) Result.t =
    try Ok (Month_ranges.Flatten.flatten_list months)
    with Range.Range_is_invalid -> Error ()

  let flatten_weekday_ranges (weekdays : weekday range Seq.t) :
    (weekday Seq.t, unit) Result.t =
    try Ok (Weekday_ranges.Flatten.flatten weekdays)
    with Range.Range_is_invalid -> Error ()

  let flatten_weekday_range_list (weekdays : weekday range list) :
    (weekday list, unit) Result.t =
    try Ok (Weekday_ranges.Flatten.flatten_list weekdays)
    with Range.Range_is_invalid -> Error ()
end
