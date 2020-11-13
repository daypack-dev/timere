type unix_second = int64

type interval = int64 * int64

type tz_offset_s = int

exception Interval_is_invalid

exception Interval_is_empty

exception Intervals_are_not_sorted

exception Intervals_are_not_disjoint

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

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

module Duration : sig
  type t = private {
    days : int;
    hours : int;
    minutes : int;
    seconds : int;
  }

  val make :
    days:int -> hours:int -> minutes:int -> seconds:int -> (t, unit) result

  val make_frac :
    days:float ->
    hours:float ->
    minutes:float ->
    seconds:int ->
    (t, unit) result

  val zero : t

  val of_seconds : int64 -> (t, unit) result

  val to_seconds : t -> int64

  val normalize : t -> t
end

type t

val of_pattern :
  ?years:int list ->
  ?months:month list ->
  ?month_days:int list ->
  ?weekdays:weekday list ->
  ?hours:int list ->
  ?minutes:int list ->
  ?seconds:int list ->
  ?unix_seconds:int64 list ->
  unit ->
  (t, unit) result

val of_years : int list -> (t, unit) result

val of_months : month list -> (t, unit) result

val of_month_days : int list -> (t, unit) result

val of_weekdays : weekday list -> (t, unit) result

val of_hours : int list -> (t, unit) result

val of_minutes : int list -> (t, unit) result

val of_seconds : int list -> (t, unit) result

val wildcard : t

val of_date_time :
  year:int ->
  month:month ->
  day:int ->
  hour:int ->
  minute:int ->
  second:int ->
  tz_offset_s:int ->
  (t, unit) result

val of_unix_second_interval : int64 * int64 -> (t, unit) result

val of_sorted_unix_second_intervals : ?skip_invalid:bool -> interval list -> t

val of_sorted_unix_second_interval_seq :
  ?skip_invalid:bool -> interval Seq.t -> t

val of_unsorted_unix_second_intervals : ?skip_invalid:bool -> interval list -> t

val of_unsorted_unix_second_interval_seq :
  ?skip_invalid:bool -> interval Seq.t -> t

val chunk : ?drop_partial:bool -> int64 -> t -> t

val shift : Duration.t -> t -> t

val lengthen : Duration.t -> t -> t

val inter : t -> t -> t

val union : t -> t -> t

val not_in : t -> t

val interval_inc : t -> t -> t

val interval_exc : t -> t -> t

val intervals_inc : t -> t -> t

val intervals_exc : t -> t -> t

val round_robin_pick : t list -> t

val round_robin_pick_seq : t Seq.t -> t

val merge : t list -> t

val merge_seq : t Seq.t -> t

val first : t -> t

val take_n : int -> t -> t

val skip_n : int -> t -> t

val first_point : t -> t

val take_n_points : int -> t -> t

val skip_n_points : int -> t -> t

module Infix : sig
  val ( &&& ) : t -> t -> t

  val ( ||| ) : t -> t -> t

  val ( -- ) : t -> t -> t

  val ( --^ ) : t -> t -> t

  val ( --* ) : t -> t -> t

  val ( --*^ ) : t -> t -> t
end

module Resolver : sig
  module Search_in_intervals : sig
    val resolve :
      ?search_using_tz_offset_s:tz_offset_s ->
      interval list ->
      Time.t ->
      (interval Seq.t, string) result
  end

  module Search_years_ahead : sig
    type start =
      [ `Unix_second of int64
      | `Date_time of Time.Date_time.t
      ]

    val resolve :
      ?search_using_tz_offset_s:tz_offset_s ->
      ?start:start ->
      int ->
      Time.t ->
      (interval Seq.t, string) result
  end
end
