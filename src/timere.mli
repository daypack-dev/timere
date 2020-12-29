type t
(** This is the core type of Timere used to encode computation over time.

    The following documentation may call value of type [t] "a Timere object".
*)

exception Invalid_format_string of string
(** Printing exception *)

type tz_offset_s = int

type timestamp = int64

type 'a range =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

(** {1 Basic constructors} *)

val always : t
(** Entire interval that Timere can handle, i.e. [\[min_timestamp, max_timestamp)] *)

val empty : t
(** Empty interval *)

val years : int list -> t
(** [years l] is a shorthand for [pattern ~years:l ()] *)

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
(** [months l] is a shorthand for [pattern ~months:l ()] *)

val month_days : int list -> t
(** [month_days l] is a shorthand for [pattern ~month_days:l ()] *)

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
(** [weekdays l] is a shorthand for [pattern ~weekdays:l ()] *)

val hours : int list -> t
(** [hours l] is a shorthand for [pattern ~hours:l ()] *)

val minutes : int list -> t
(** [minutes l] is a shorthand for [pattern ~minutes:l ()] *)

val seconds : int list -> t
(** [seconds l] is a shorthand for [pattern ~seconds:l ()] *)

val pattern :
  ?years:int list ->
  ?year_ranges:int range list ->
  ?months:month list ->
  ?month_ranges:month range list ->
  ?month_days:int list ->
  ?month_day_ranges:int range list ->
  ?weekdays:weekday list ->
  ?weekday_ranges:weekday range list ->
  ?hours:int list ->
  ?hour_ranges:int range list ->
  ?minutes:int list ->
  ?minute_ranges:int range list ->
  ?seconds:int list ->
  ?second_ranges:int range list ->
  unit ->
  t
(** Pattern matches over date times.

    A pattern [p] matches date time [dt] if
    {[
      (dt.year is in p.years or p.year_ranges)
      && (dt.month is in p.months or p.month_ranges)
      && (dt.month_day is in p.month_days or p.month_day_ranges)
      && (dt.weekday is in p.weekdays or p.weekday_ranges)
      && (dt.hour is in p.hours or p.hour_ranges)
      && (dt.minute is in p.minutes or p.minute_ranges)
      && (dt.second is in p.seconds or p.second_ranges)
    ]}

    Empty pattern levels are treated as wildcard, e.g. if [p.years] and [p.year_ranges] are both empty,
    then [(dt.year is in p.years or p.year_ranges)] is [true].
*)

(** {1 Time zone} *)

module Time_zone : sig
  type t

  val make : string -> (t, unit) result
  (** Makes a time zone from name.

      Naming follows the convention used in [/usr/share/zoneinfo/posix/] distributed on Linux, e.g. "Australia/Sydney".

      See {{:https://github.com/daypack-dev/timere/tree/main/gen_artifacts/available-time-zones.txt} [available-time-zones.txt]} or {!val:available_time_zones} or for all usable time zone names.
  *)

  val make_exn : string -> t
  (** @raise Invalid_argument if [make] fails *)

  val name : t -> string

  val utc : t

  val available_time_zones : string list
end

val with_tz : Time_zone.t -> t -> t
(** [with_tz tz t] changes the time zone to evaluate [t] in to [tz] *)

(** {1 Algebraic operations} *)

val inter : t list -> t

val union : t list -> t

val not : t -> t

(** {1 Duration} *)

module Duration : sig
  type t = private {
    days : int;
    hours : int;
    minutes : int;
    seconds : int;
  }

  val make :
    ?days:int -> ?hours:int -> ?minutes:int -> ?seconds:int -> unit -> t
  (** @raise Invalid_argument if any of the arguments are negative *)

  val make_frac :
    ?days:float -> ?hours:float -> ?minutes:float -> ?seconds:int -> unit -> t
  (** @raise Invalid_argument if any of the arguments are negative *)

  val zero : t

  val of_seconds : int64 -> t
  (** @raise Invalid_argument if argument is negative *)

  val to_seconds : t -> int64

  val sprint : t -> string

  val pp : Format.formatter -> t -> unit
end

val shift : Duration.t -> t -> t

val lengthen : Duration.t -> t -> t

(** {1 Date time and timestamps} *)

module Date_time : sig
  type tz_info =
    [ `Tz_only of Time_zone.t
    | `Tz_offset_s_only of int
    | `Tz_and_tz_offset_s of Time_zone.t * int
    ]

  type t = private {
    year : int;
    month : month;
    day : int;
    hour : int;
    minute : int;
    second : int;
    tz_info : tz_info;
  }

  val make :
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
    tz:Time_zone.t ->
    (t, unit) result
  (** Constructs a date time providing only a time zone.

      A precise offset is inferred if possible.

      Note that this may yield a ambiguous date time if the time zone has varying offsets, e.g. DST.

      See {!val:make_precise} for the most precise construction.
  *)

  val make_exn :
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
    tz:Time_zone.t ->
    t
  (** @raise Invalid_argument if [make] fails *)

  val make_precise :
    ?tz:Time_zone.t ->
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
    tz_offset_s:tz_offset_s ->
    unit ->
    (t, unit) result
  (** Constructs a date time providing time zone offset in seconds, and optionally a time zone.

      If a time zone is provided, then the offset is checked against the time zone record to make sure
      the time zone does use said offset for the particular date time.
  *)

  val make_precise_exn :
    ?tz:Time_zone.t ->
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
    tz_offset_s:tz_offset_s ->
    unit ->
    t
  (** @raise Invalid_argument if [make_precise] fails *)

  type 'a local_result =
    [ `None
    | `Exact of 'a
    | `Ambiguous of 'a * 'a
    ]
  (** Result for when a local date time may be involved, e.g. using a date time with no precise time zone offset attached.

      - [`None] is yielded when the date time does not map to any ['a].
        This happens when DST begins and "skips an hour" for instance.
      - [`Exact] is yielded when the date time maps to exactly one ['a].
        This happens when date time carries an accurate offset,
        or when the date time is not affected by any offset shifts (thus an accurate offset can be inferred).
      - [`Ambiguous] is yielded when date time maps to more than one (exactly two) ['a].
        This happens when DST ends and "goes back an hour" for instance.
  *)

  val to_timestamp : t -> timestamp local_result

  val to_timestamp_exact : t -> timestamp
  (** @raise Invalid_argument if [to_timestamp] does not yield a [`Exact] result *)

  val min_of_timestamp_local_result : timestamp local_result -> timestamp option

  val max_of_timestamp_local_result : timestamp local_result -> timestamp option

  val of_timestamp :
    ?tz_of_date_time:Time_zone.t -> timestamp -> (t, unit) result

  val equal : t -> t -> bool

  val min : t

  val max : t

  val cur : ?tz_of_date_time:Time_zone.t -> unit -> (t, unit) result

  val sprintf : ?format:string -> t -> string

  val pp : ?format:string -> Format.formatter -> t -> unit

  val to_rfc3339 : t -> string

  val of_iso8601 : string -> (t, string) result
end

val cur_timestamp : unit -> int64

val min_timestamp : int64

val max_timestamp : int64

exception Invalid_timestamp

val of_timestamps : ?skip_invalid:bool -> timestamp list -> t

val of_timestamp_seq : ?skip_invalid:bool -> timestamp Seq.t -> t

(** {1 Chunking} *)

type chunked

type chunking =
  [ `Disjoint_intervals
  | `By_duration of Duration.t
  | `By_duration_drop_partial of Duration.t
  | `At_year_boundary
  | `At_month_boundary
  ]
(** Ways to chunk/slice time intervals for the selector.

    - [`Disjoint_intervals] gives a sequence of disjoint intervals to the selector,
      specifically they are in ascending order, non-overlapping, non-connecting, and unique
    - [`By_duration] slices in the fixed size specified by the duration.
      Partial chunks (chunks less than the fixed size) are preserved.
    - [`By_duration_drop_partial] slices in the fixed size specified by the duration.
      Partial chunks (chunks less than the fixed size) are discarded.
    - [`At_year_boundary] slices at the year boundary (e.g. [2021 Jan 1st 00:00:00])
    - [`At_month_boundary] slices at the month boundary (e.g. [Aug 1st 00:00:00])
*)

val chunk : chunking -> (chunked -> chunked) -> t -> t
(** [chunk chunking f t] applies [chunked] selector [f] on [t]*)

val chunk_again : chunking -> chunked -> chunked
(** [chunk_again chunking f] applies [chunked] selector [f] as a selector*)

(** {2 Chunked selectors} *)

(** You may find {!val:Infix.(>>)} useful for chaining selectors together, e.g. [drop 5 >> take 2]
*)

val first : chunked -> chunked
(** Takes only first chunk *)

val take : int -> chunked -> chunked
(** Takes n chunks *)

val take_nth : int -> chunked -> chunked
(** Take every nth chunk, specifically [0]th, [n]th, [2n]th, [3n]th, ... *)

val drop : int -> chunked -> chunked
(** Discard n chunks *)

(* val first_point : t -> t
 * 
 * val take_n_points : int -> t -> t
 * 
 * val drop_n_points : int -> t -> t *)

(** {1 Manual intervals} *)

val interval_dt_inc : Date_time.t -> Date_time.t -> t

val interval_dt_exc : Date_time.t -> Date_time.t -> t

val interval_inc : timestamp -> timestamp -> t

val interval_exc : timestamp -> timestamp -> t

exception Interval_is_invalid

exception Interval_is_empty

exception Intervals_are_not_sorted

exception Intervals_are_not_disjoint

type interval = timestamp * timestamp

val of_intervals : ?skip_invalid:bool -> interval list -> t

val of_interval_seq : ?skip_invalid:bool -> interval Seq.t -> t

val of_sorted_intervals : ?skip_invalid:bool -> interval list -> t

val of_sorted_interval_seq : ?skip_invalid:bool -> interval Seq.t -> t

(** {2 Hour minute second intervals} *)

type hms = private {
  hour : int;
  minute : int;
  second : int;
}

val make_hms : hour:int -> minute:int -> second:int -> (hms, unit) result

val make_hms_exn : hour:int -> minute:int -> second:int -> hms

val hms_interval_inc : hms -> hms -> t

val hms_interval_exc : hms -> hms -> t

val of_hms_intervals : (hms * hms) Seq.t -> t

(* (\** {1 Search oriented operations} *\)
 * 
 * val after : Duration.t -> t -> t -> t
 * 
 * val between_inc : Duration.t -> t -> t -> t
 * 
 * val between_exc : Duration.t -> t -> t -> t *)

(** {1 Infix operators} *)

module Infix : sig
  val ( & ) : t -> t -> t
  (** [inter] *)

  val ( ||| ) : t -> t -> t
  (** [union] *)

  val ( >> ) : ('a -> 'a) -> ('a -> 'a) -> 'a -> 'a
  (** Composition, mainly for chunked selectors

      [f1 >> f2] is equivalent to [fun x -> x |> f1 |> f2].
  *)
end

(** {1 Resolution} *)

val resolve :
  ?search_using_tz:Time_zone.t -> t -> (interval Seq.t, string) result
(** Resolves a Timere object into a concrete interval sequence *)

(** {1 Pretty printers} *)

val sprintf_timestamp :
  ?display_using_tz:Time_zone.t -> ?format:string -> timestamp -> string

val pp_timestamp :
  ?display_using_tz:Time_zone.t ->
  ?format:string ->
  Format.formatter ->
  timestamp ->
  unit

val sprintf_interval :
  ?display_using_tz:Time_zone.t -> ?format:string -> interval -> string

val pp_interval :
  ?display_using_tz:Time_zone.t ->
  ?format:string ->
  Format.formatter ->
  interval ->
  unit

(** {1 S-expressions for serialization/deserialization} *)

val to_sexp : t -> CCSexp.t

val pp_sexp : Format.formatter -> t -> unit

val to_sexp_string : t -> string

val of_sexp : CCSexp.t -> (t, string) result

val of_sexp_string : string -> (t, string) result

(** {1 Misc} *)

module Utils : sig
  val flatten_month_ranges : month range Seq.t -> (month Seq.t, unit) result

  val flatten_month_day_ranges : int range Seq.t -> (int Seq.t, unit) result

  val flatten_weekday_ranges :
    weekday range Seq.t -> (weekday Seq.t, unit) result

  val flatten_month_range_list : month range list -> (month list, unit) result

  val flatten_month_day_range_list : int range list -> (int list, unit) result

  val flatten_weekday_range_list :
    weekday range list -> (weekday list, unit) result

  val resolve_simple :
    ?search_using_tz:Time_zone.t ->
    search_start:timestamp ->
    search_end_exc:timestamp ->
    t ->
    interval Seq.t
end