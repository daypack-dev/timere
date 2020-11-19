module Duration = Duration
include Time

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
  let flatten_month_ranges (months : month range Seq.t) : (month Seq.t, unit) Result.t =
    Month_ranges.Flatten.flatten
end
