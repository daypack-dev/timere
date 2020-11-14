module Resolver = Resolver
module Duration = Duration
include Time

type interval = Time.Interval.t

module Infix = struct
  let ( &&& ) = Time.inter

  let ( ||| ) = Time.union

  let ( -- ) = Time.interval_inc

  let ( --^ ) = Time.interval_exc

  let ( --* ) = Time.intervals_inc

  let ( --*^ ) = Time.intervals_exc
end
