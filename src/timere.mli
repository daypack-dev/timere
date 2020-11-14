type tz_offset_s = int

type timestamp = int64

type t

val any : t

val years : int list -> t

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

val months : month list -> t

val month_days : int list -> t

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

val weekdays : weekday list -> t

val hours : int list -> t

val minutes : int list -> t

val seconds : int list -> t

val pattern :
  ?years:int list ->
  ?months:month list ->
  ?month_days:int list ->
  ?weekdays:weekday list ->
  ?hours:int list ->
  ?minutes:int list ->
  ?seconds:int list ->
  ?timestamps:timestamp list ->
  unit ->
  t

(** {1 Algebraic operations} *)

val inter : t -> t -> t

val union : t -> t -> t

val not : t -> t

val interval_inc : t -> t -> t

val interval_exc : t -> t -> t

val intervals_inc : t -> t -> t

val intervals_exc : t -> t -> t

val merge : t list -> t

(** {1 Discrete time points} *)

module Date_time : sig
  type t = private {
    year : int;
    month : month;
    day : int;
    hour : int;
    minute : int;
    second : int;
    tz_offset_s : int;
  }

  val make :
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
    tz_offset_s:int ->
    (t, unit) result

  val to_timestamp : t -> (timestamp, unit) result

  val of_timestamp :
    tz_offset_s_of_date_time:tz_offset_s option -> timestamp -> (t, unit) result

  val compare : t -> t -> int

  val min : t

  val max : t
end

val of_date_time : Date_time.t -> (t, unit) result

(** {1 Durations} *)

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
end

val shift : Duration.t -> t -> t

val lengthen : Duration.t -> t -> t

(** {1 List and Filtering operations} *)

val chunk : ?drop_partial:bool -> int64 -> t -> t

val first : t -> t

val take_n : int -> t -> t

val skip_n : int -> t -> t

val first_point : t -> t

val take_n_points : int -> t -> t

val skip_n_points : int -> t -> t

(** {1 Manual intervals} *)

exception Interval_is_invalid

exception Interval_is_empty

exception Intervals_are_not_sorted

exception Intervals_are_not_disjoint

type interval = timestamp * timestamp

val of_interval : interval -> t

val of_intervals : ?skip_invalid:bool -> interval list -> t

val of_intervals_seq : ?skip_invalid:bool -> interval Seq.t -> t

val of_sorted_intervals : ?skip_invalid:bool -> interval list -> t

val of_sorted_intervals_seq : ?skip_invalid:bool -> interval Seq.t -> t

(** {1 Sampling} *)

val round_robin_pick : t list -> t

(** {1 Infix operators} *)
module Infix : sig
  val ( &&& ) : t -> t -> t

  val ( ||| ) : t -> t -> t

  val ( -- ) : t -> t -> t

  val ( --^ ) : t -> t -> t

  val ( --* ) : t -> t -> t

  val ( --*^ ) : t -> t -> t
end

val resolve :
  ?search_using_tz_offset_s:tz_offset_s -> t -> (interval Seq.t, string) result
