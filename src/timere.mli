type tz_offset_s = int

type timestamp = int64

type 'a range =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

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

type hms = {
  hour : int;
  minute : int;
  second : int;
}

type branching_days =
  | Month_days of int range list
  | Weekdays of weekday range list

val branching :
  ?years:int range list ->
  ?months:month range list ->
  ?days:branching_days ->
  ?hmss:hms range list ->
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
  val ( & ) : t -> t -> t

  val ( ||| ) : t -> t -> t

  val ( -- ) : t -> t -> t

  val ( --^ ) : t -> t -> t

  val ( --* ) : t -> t -> t

  val ( --*^ ) : t -> t -> t
end

val resolve :
  ?search_using_tz_offset_s:tz_offset_s -> t -> (interval Seq.t, string) result

(** {1 Low-level range handling}*)
module Range : sig
  exception Modulo_is_invalid

  exception Range_is_invalid

  module type B = sig
    type t

    val modulo : int64 option

    val to_int64 : t -> int64

    val of_int64 : int64 -> t
  end

  module type S = sig
    type t

    val int64_range_of_range : t range -> int64 range

    val int64_inc_range_of_range : t range -> int64 * int64

    val int64_exc_range_of_range : t range -> int64 * int64

    val inc_range_of_range : t range -> t * t

    val exc_range_of_range : t range -> t * t

    val join : t range -> t range -> t range option

    val is_valid : t range -> bool

    module Flatten : sig
      val flatten_into_seq : t range -> t Seq.t

      val flatten_into_list : t range -> t list
    end
  end

  module Make (B : B) : S with type t := B.t
end

module Range_small : sig
  module type B = sig
    type t

    val modulo : int option

    val to_int : t -> int

    val of_int : int -> t
  end

  module type S = sig
    type t

    val int_range_of_range : t range -> int range

    val int_exc_range_of_range : t range -> int * int

    val inc_range_of_range : t range -> t * t

    val exc_range_of_range : t range -> t * t

    val join : t range -> t range -> t range option

    module Flatten : sig
      val flatten_into_seq : t range -> t Seq.t

      val flatten_into_list : t range -> t list
    end
  end

  module Make (B : B) : S with type t := B.t
end

module Ranges : sig
  module type S = sig
    type t

    val normalize :
      ?skip_filter_invalid:bool ->
      ?skip_filter_empty:bool ->
      ?skip_sort:bool ->
      t range Seq.t ->
      t range Seq.t

    module Check : sig
      val seq_is_valid : t range Seq.t -> bool

      val list_is_valid : t range list -> bool
    end

    module Flatten : sig
      val flatten : t range Seq.t -> t Seq.t

      val flatten_list : t range list -> t list
    end

    module Of_seq : sig
      val range_seq_of_seq : t Seq.t -> t range Seq.t

      val range_list_of_seq : t Seq.t -> t range list
    end

    module Of_list : sig
      val range_seq_of_list : t list -> t range Seq.t

      val range_list_of_list : t list -> t range list
    end
  end

  module Make (B : Range.B) : S with type t := B.t
end

module Ranges_small : sig
  module Make (B : Range_small.B) : Ranges.S with type t := B.t
end

module Second_ranges : Ranges.S with type t := int

module Minute_ranges : Ranges.S with type t := int

module Hour_ranges : Ranges.S with type t := int

module Weekday_ranges : Ranges.S with type t := weekday

module Month_day_ranges : Ranges.S with type t := int

module Month_ranges : Ranges.S with type t := month

module Year_ranges : Ranges.S with type t := int
