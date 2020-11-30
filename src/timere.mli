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

exception Month_day_ranges_are_invalid

val branching :
  ?allow_out_of_range_month_day:bool ->
  ?years:int range list ->
  ?months:month range list ->
  ?days:branching_days ->
  ?hmss:hms range list ->
  unit ->
  t

(** {1 Timezone change} *)

val to_tz_offset_s : int -> t -> t

(** {1 Algebraic operations} *)

val inter : t -> t -> t

val union : t -> t -> t

val not : t -> t

val merge : t list -> t

(** {1 Discrete time points} *)

val cur_timestamp : unit -> int64

module Date_time : sig
  exception Invalid_date_time

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

  val make_exn :
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
    tz_offset_s:int ->
    t

  val to_timestamp : t -> timestamp

  val of_timestamp :
    ?tz_offset_s_of_date_time:tz_offset_s -> timestamp -> (t, unit) result

  val compare : t -> t -> int

  val min : t

  val max : t

  val cur : ?tz_offset_s_of_date_time:tz_offset_s -> unit -> (t, unit) result

  val sprintf : string -> t -> (string, string) result

  val pp : string -> Format.formatter -> t -> unit
end

val date_time : Date_time.t -> t

val interval_inc : Date_time.t -> Date_time.t -> t

val interval_exc : Date_time.t -> Date_time.t -> t

(** {1 Durations} *)

module Duration : sig
  type t = private {
    days : int;
    hours : int;
    minutes : int;
    seconds : int;
  }

  val make :
    ?days:int ->
    ?hours:int ->
    ?minutes:int ->
    ?seconds:int ->
    unit ->
    (t, unit) result

  val make_frac :
    ?days:float ->
    ?hours:float ->
    ?minutes:float ->
    ?seconds:int ->
    unit ->
    (t, unit) result

  val zero : t

  val of_seconds : int64 -> (t, unit) result

  val to_seconds : t -> int64

  val sprint : t -> string

  val pp : Format.formatter -> t -> unit
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
end

val resolve :
  ?search_using_tz_offset_s:tz_offset_s -> t -> (interval Seq.t, string) result

(** {1 Pretty printers} *)

val sprintf_timestamp :
  ?display_using_tz_offset_s:tz_offset_s ->
  string ->
  timestamp ->
  (string, string) result

val pp_timestamp :
  ?display_using_tz_offset_s:tz_offset_s ->
  string ->
  Format.formatter ->
  timestamp ->
  unit

val sprintf_interval :
  ?display_using_tz_offset_s:tz_offset_s ->
  string ->
  interval ->
  (string, string) result

val pp_interval :
  ?display_using_tz_offset_s:tz_offset_s ->
  string ->
  Format.formatter ->
  interval ->
  unit

(** {1 S-expressions} *)

val to_sexp : t -> CCSexp.t

val pp_sexp : Format.formatter -> t -> unit

val to_sexp_string : t -> string

module Utils : sig
  val flatten_month_ranges : month range Seq.t -> (month Seq.t, unit) result

  val flatten_weekday_ranges :
    weekday range Seq.t -> (weekday Seq.t, unit) result

  val flatten_month_range_list : month range list -> (month list, unit) result

  val flatten_weekday_range_list :
    weekday range list -> (weekday list, unit) result
end
