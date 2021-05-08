include Time_ast
include Time
include Infix

type timestamp = Timedesc.timestamp

module Hms = struct
  include Hms'

  let pp = Printers.pp_hms

  let to_string = Printers.string_of_hms
end

type 'a range = 'a Range.range

module Points = Points

type points = Points.t

let resolve = Resolver.resolve

let to_sexp = To_sexp.to_sexp

let to_sexp_string = To_sexp.to_sexp_string

let of_sexp = Of_sexp.(wrap_of_sexp of_sexp)

let of_sexp_string = Of_sexp.of_sexp_string

let pp_sexp = Printers.pp_sexp

module Utils = struct
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

  let flatten_weekday_ranges (weekdays : Timedesc.weekday range Seq.t) :
    Timedesc.weekday Seq.t option =
    try Some (Weekday_ranges.Flatten.flatten weekdays)
    with Range.Range_is_invalid -> None

  let flatten_weekday_range_list (weekdays : Timedesc.weekday range list) :
    Timedesc.weekday list option =
    try Some (Weekday_ranges.Flatten.flatten_list weekdays)
    with Range.Range_is_invalid -> None
end
