type unix_second = int64

type interval = int64 * int64

type tz_offset_s = int

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

val normalize :
  ?skip_filter_invalid:bool ->
  ?skip_filter_empty:bool ->
  ?skip_sort:bool ->
  t ->
  t

val chunk : ?drop_partial:bool -> int64 -> t -> t

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

val of_date_time :
  year:int ->
  month:month ->
  day:int ->
  hour:int ->
  minute:int ->
  second:int ->
  tz_offset_s:int ->
  (t, unit) result

val of_unix_second_interval : int64 * int64 -> t

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
