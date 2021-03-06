type t
(** This is the core type of Timere used to encode computation over time.

    The following documentation may call value of type [t] "a Timere object", or "timere".
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

val now : unit -> t
(** Time right now *)

val always : t
(** Entire interval that Timere can handle, i.e. [\[0000 Jan 01 14:00:00 +00:00:00, 9999 Dec 31 09:59:58 +00:00:00)] *)

val empty : t
(** Empty interval *)

val years : int list -> t
(** [years l] is a shorthand for [pattern ~years:l ()] *)

val year_ranges : int range list -> t
(** [year_ranges l] is a shorthand for [pattern ~year_ranges:l ()] *)

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

val month_ranges : month range list -> t
(** [month_ranges l] is a shorthand for [pattern ~month_ranges:l ()] *)

val days : int list -> t
(** [days l] is a shorthand for [pattern ~month_days:l ()] *)

val day_ranges : int range list -> t
(** [day_ranges l] is a shorthand for [pattern ~month_day_ranges:l ()] *)

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

val weekday_ranges : weekday range list -> t
(** [weekday_ranges l] is a shorthand for [pattern ~weekday_ranges:l ()] *)

val hours : int list -> t
(** [hours l] is a shorthand for [pattern ~hours:l ()] *)

val hour_ranges : int range list -> t
(** [hour_ranges l] is a shorthand for [pattern ~hour_ranges:l ()] *)

val minutes : int list -> t
(** [minutes l] is a shorthand for [pattern ~minutes:l ()] *)

val minute_ranges : int range list -> t
(** [minute_ranges l] is a shorthand for [pattern ~minute_ranges:l ()] *)

val seconds : int list -> t
(** [seconds l] is a shorthand for [pattern ~seconds:l ()] *)

val second_ranges : int range list -> t
(** [second_ranges l] is a shorthand for [pattern ~second_ranges:l ()] *)

val pattern :
  ?years:int list ->
  ?year_ranges:int range list ->
  ?months:month list ->
  ?month_ranges:month range list ->
  ?days:int list ->
  ?day_ranges:int range list ->
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
    {v
(dt.year is in p.years or p.year_ranges)
&& (dt.month is in p.months or p.month_ranges)
&& (dt.month_day is in p.month_days or p.month_day_ranges)
&& (dt.weekday is in p.weekdays or p.weekday_ranges)
&& (dt.hour is in p.hours or p.hour_ranges)
&& (dt.minute is in p.minutes or p.minute_ranges)
&& (dt.second is in p.seconds or p.second_ranges)
    v}

    Empty pattern levels are treated as wildcard, e.g. if [p.years] and [p.year_ranges] are both empty,
    then [(dt.year is in p.years or p.year_ranges)] is [true].
*)

val nth_weekday_of_month : int -> weekday -> t
(** [nth_weekday_of_month n wday] picks the nth weekday of all months, where [1 <= n && n <= 5]

    @raise Invalid_argument if [n] is out of range
*)

(** {1 Algebraic operations} *)

val inter : t list -> t
(** Intersection of list of timeres.

    [inter []] is equivalent to [empty].
*)

val union : t list -> t
(** Union of list of timeres.

    [union []] is equivalent to [empty].
*)

val not : t -> t
(** Negation of timere.

    [not t] is equivalent to all the intervals not included in [t].
*)

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

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end

val shift : Duration.t -> t -> t

val lengthen : Duration.t -> t -> t

(** {1 Time zone} *)

module Time_zone : sig
  type t

  val make : string -> t option
  (** Makes a time zone from name.

      Naming follows the convention used in [/usr/share/zoneinfo/posix/] distributed on Linux, e.g. "Australia/Sydney".

      See {!val:available_time_zones} or checking usable time zone names at runtime.

      Alternatively, if you are using [timere.tz.full] (the default implementation for [timere.tz.data]), then you can also see
      {{:https://github.com/daypack-dev/timere/tree/main/gen_artifacts/available-time-zones.txt} [available-time-zones.txt]} for available time zones.
  *)

  val make_exn : string -> t
  (** @raise Invalid_argument if [make] fails *)

  val name : t -> string

  val utc : t

  val local : unit -> t option

  val equal : t -> t -> bool

  val available_time_zones : string list

  val make_offset_only : ?name:string -> int -> t
  (** This is mainly used for when you only have an offset to work with,
      and you don't need to do any accurate search over time zones.

      One use of this is to create a time zone for [to_string] functions.
  *)

  (** {2 Importing and exporting}*)

  type entry = {
    is_dst : bool;
    offset : int;
  }

  module Raw : sig
    val of_transitions : name:string -> (int64 * entry) list -> t option

    val to_transitions : t -> ((int64 * int64) * entry) list

    val to_transition_seq : t -> ((int64 * int64) * entry) Seq.t
  end

  module Sexp : sig
    val to_sexp : t -> CCSexp.t

    val of_sexp : CCSexp.t -> t option

    val of_string : string -> t option
  end

  module JSON : sig
    val to_json : t -> Yojson.Basic.t

    val of_json : Yojson.Basic.t -> t option

    val of_string : string -> t option
  end

  module Db : sig
    type db

    val empty : db

    val add : t -> db -> db

    val find_opt : string -> db -> t option

    val remove : string -> db -> db

    val of_seq : t Seq.t -> db

    val add_seq : db -> t Seq.t -> db

    val names : db -> string list

    module Raw : sig
      val dump : db -> string

      val load : string -> db
    end

    module Sexp : sig
      val of_sexp : CCSexp.t -> db option

      val to_sexp : db -> CCSexp.t

      val of_string : string -> db option
    end
  end
end

val with_tz : Time_zone.t -> t -> t
(** [with_tz tz t] changes the time zone to evaluate [t] in to [tz] *)

(** {1 Date time and timestamps} *)

module Date_time : sig
  type tz_info =
    private
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
    tz:Time_zone.t ->
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
    t option
  (** Constructs a date time providing only a time zone.

      A precise offset is inferred if possible.

      Note that this may yield a ambiguous date time if the time zone has varying offsets, e.g. DST.

      See {!val:make_precise} for the most precise construction.
  *)

  val make_exn :
    tz:Time_zone.t ->
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
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
    t option
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
    [ `Single of 'a
    | `Ambiguous of 'a * 'a
    ]
  (** Result for when a local date time may be involved, e.g. using a date time with no precise time zone offset attached.

      - [`Single] is yielded when the date time maps to exactly one ['a].
        This happens when date time carries an accurate offset,
        or when the date time is not affected by any offset shifts (thus an accurate offset can be inferred).
      - [`Ambiguous] is yielded when date time maps to more than one (exactly two) ['a].
        This happens when DST ends and "goes back an hour" for instance.
  *)

  val to_timestamp : t -> timestamp local_result

  val to_timestamp_single : t -> timestamp
  (** @raise Invalid_argument if [to_timestamp] does not yield a [`Single] result *)

  val min_of_timestamp_local_result : timestamp local_result -> timestamp

  val max_of_timestamp_local_result : timestamp local_result -> timestamp

  val of_timestamp : ?tz_of_date_time:Time_zone.t -> timestamp -> t option

  val equal : t -> t -> bool

  val min : t

  val max : t

  val now : ?tz_of_date_time:Time_zone.t -> unit -> t

  val to_string : ?format:string -> t -> string
  (**
     Pretty printing for date time.

     Default format string:
     {v
{year} {mon:Xxx} {mday:0X} {hour:0X}:{min:0X}:{sec:0X} \
{tzoff-sign}{tzoff-hour:0X}:{tzoff-min:0X}:{tzoff-sec:0X}
     v}

     Format string specification:
     {v
{{               literal {
{year}           year
{mon:Xxx}        abbreviated month name (e.g. Jan), casing of 'x' controls the casing
{mon:Xx*}        full month name (e.g. January), casing of first 'x' controls casing of first letter,
                 casing of second 'x' controls casing of following letters
{mday:cX}        month day (e.g.  1) character 'c' before 'X' is used for padding
                 (leave out character for no padding)
{wday:Xxx}       abbreviated weekday name (e.g. Sun), the casing of 'x' controls the casing
{wday:Xx*}       full weekday name (e.g. Sunday), casing of first 'x' controls casing of first letter,
                 casing of second 'x' controls casing of following letters
{hour:cX}        hour in 24-hour format, character 'c' before 'X' determines padding
                 (leave out character for no padding)
{12hour:cX}      hour in 12-hour format, character 'c' before 'X' determines padding
                 (leave out character for no padding)
{min:cX}         minute, character 'c' before 'X' determines padding
                 (leave out character for no padding)
{sec:cX}         second, character 'c' before 'X' determines padding
                 (leave out character for no padding)
{tzoff-sign}     time zone offset sign ('+' or '-')
                 yields "N/A" if time zone offset is not available
{tzoff-hour:cX}  time zone offset hour, follows same padding rule as "{hour:cX}"
                 yields "N/A" if time zone offset is not available
{tzoff-min:cX}   time zone offset minute, follows same padding rule as "{min:cX}"
                 yields "N/A" if time zone offset is not available
{tzoff-sec:cX}   time zone offset second, follows same padding rule as "{sec:cX}"
                 yields "N/A" if time zone offset is not available
     v}
  *)

  val pp : ?format:string -> unit -> Format.formatter -> t -> unit

  val to_rfc3339 : t -> string

  val of_iso8601 : string -> (t, string) result
end

val date_time : Date_time.t -> t

val before : Date_time.t -> t

val after : Date_time.t -> t

val date_times : Date_time.t list -> t

val date_time_seq : Date_time.t Seq.t -> t

val sorted_date_times : Date_time.t list -> t

val sorted_date_time_seq : Date_time.t Seq.t -> t

val timestamp_now : unit -> timestamp

val timestamp_min : int64

val timestamp_max : int64

exception Invalid_timestamp

val timestamp : timestamp -> t

val before_timestamp : timestamp -> t

val after_timestamp : timestamp -> t

val timestamps : ?skip_invalid:bool -> timestamp list -> t
(** [timestamps l]

    [skip_invalid] defaults to [false]

    @raise Invalid_timestamp if [not skip_invalid] and [l] contains an invalid timestamp
*)

val timestamp_seq : ?skip_invalid:bool -> timestamp Seq.t -> t
(** [timestamps s]

    [skip_invalid] defaults to [false]

    @raise Invalid_timestamp if [not skip_invalid] and [s] contains an invalid timestamp
*)

val sorted_timestamps : ?skip_invalid:bool -> timestamp list -> t

val sorted_timestamp_seq : ?skip_invalid:bool -> timestamp Seq.t -> t

(** {1 Intervals} *)

(** {2 Explicit} *)

exception Interval_is_invalid

exception Intervals_are_not_sorted

type interval = timestamp * timestamp

val intervals : ?skip_invalid:bool -> interval list -> t
(** [intervals l]

    [skip_invalid] defaults to [false]

    @raise Interval_is_invalid if [not skip_invalid] and [l] contains an invalid interval
*)

val interval_seq : ?skip_invalid:bool -> interval Seq.t -> t
(** [interval_seq s]

    [skip_invalid] defaults to [false]

    @raise Interval_is_invalid if [not skip_invalid] and [s] contains an invalid interval
*)

val sorted_intervals : ?skip_invalid:bool -> interval list -> t
(** [sorted_intervals l]

    [skip_invalid] defaults to [false]

    @raise Interval_is_invalid if [not skip_invalid] and [l] contains an invalid interval
    @raise Intervals_are_not_sorted if [l] is not sorted
*)

val sorted_interval_seq : ?skip_invalid:bool -> interval Seq.t -> t
(** [sorted_interval_seq s]

    [skip_invalid] defaults to [false]

    @raise Interval_is_invalid if [not skip_invalid] and [s] contains an invalid interval
    @raise Intervals_are_not_sorted if [s] is not sorted
*)

(** {2 Pattern matching} *)

(** Pattern matching intervals are designed to handle intervals where start and end points follow some pattern, but cannot be captured by [pattern] efficiently,
    e.g. you cannot represent "5:30pm to 6:11pm" via a single [pattern]
*)

type points

val make_points :
  ?tz:Time_zone.t ->
  ?tz_offset_s:int ->
  ?year:int ->
  ?month:month ->
  ?day:int ->
  ?weekday:weekday ->
  ?hour:int ->
  ?minute:int ->
  second:int ->
  unit ->
  points option
(** [make_points] call must be exactly one of the following form (ignoring [tz] and [tz_offset_s] which are optional in all cases)
    {v
make_points ~year:_ ~month:_ ~day:_     ~hour:_ ~minute:_ ~second:_ ()
make_points         ~month:_ ~day:_     ~hour:_ ~minute:_ ~second:_ ()
make_points                  ~day:_     ~hour:_ ~minute:_ ~second:_ ()
make_points                  ~weekday:_ ~hour:_ ~minute:_ ~second:_ ()
make_points                             ~hour:_ ~minute:_ ~second:_ ()
make_points                                     ~minute:_ ~second:_ ()
make_points                                               ~second:_ ()
    v}

    returns [Error] otherwise
*)

val make_points_exn :
  ?tz:Time_zone.t ->
  ?tz_offset_s:int ->
  ?year:int ->
  ?month:month ->
  ?day:int ->
  ?weekday:weekday ->
  ?hour:int ->
  ?minute:int ->
  second:int ->
  unit ->
  points
(** @raise Invalid_argument if [make_points] fails *)

val bounded_intervals : [ `Whole | `Snd ] -> Duration.t -> points -> points -> t
(** [bounded_intervals mode bound p1 p2] for each point [x] matched by [p1],
    then for each earliest point [y] matched by [p2] such that [x < y && y - x <= bound]
    - if [mode = `Whole], yields (x, y)
    - if [mode = `Snd], yields (y, y + 1)

    Examples:

    {[
      bounded_intervals `Whole (Duration.make ~days:1 ())
        (make_points ~hour:13 ~minute:0 ~second:0 ()) (* p1 *)
        (make_points ~hour:14 ~minute:0 ~second:0 ()) (* p2 *)
    ]}
    yields all the "1pm to 2pm" intervals, since at each "1pm" mark represented by [p1],
    searching forward up to 24 hour period, we can find a "2pm" mark in [p2]

    {[
      bounded_intervals `Whole (Duration.make ~days:1 ())
        (make_points ~month:`Feb ~day:10 ~hour:13 ~minute:0 ~second:0 ()) (* p1 *)
        (make_points                     ~hour:14 ~minute:0 ~second:0 ()) (* p2 *)
    ]}
    yields all the "Feb 10th 1pm to 2pm" intervals (or specifically "Feb 10th 1pm to Feb 10th 2pm")

    {[
      bounded_intervals `Whole (Duration.make ~days:1 ())
        (make_points ~month:`Feb ~day:10 ~hour:23 ~minute:0 ~second:0 ()) (* p1 *)
        (make_points                     ~hour:3  ~minute:0 ~second:0 ()) (* p2 *)
    ]}
    yields all the "Feb 10th 11pm to 3am" intervals (or specifically "Feb 10th 11pm to Feb 11th 3am")

    @raise Invalid_argument if precision (number of date time arguments passed to [make_points] during construction)
    of [p1] < precision of [p2]

    For example, [make_points_exn ~hour:3 ~minute:0 ~second:0 ()]
    has a lower precision than [make_points_exn ~day:10 ~hour:12 ~minute:30 ~second:0 ()]
*)

(** {2 Hour minute second intervals} *)

(** Convenience wrappers around [points] and [bounded_intervals] *)

type hms = private {
  hour : int;
  minute : int;
  second : int;
}

val make_hms : hour:int -> minute:int -> second:int -> hms option

val make_hms_exn : hour:int -> minute:int -> second:int -> hms

val hms_intervals_inc : hms -> hms -> t
(** Same as [hms_intervals_exc ...] with end point increased by one second
*)

val hms_intervals_exc : hms -> hms -> t
(** Same as [bounded_intervals ...] with bound fixed to [Duration.make ~days:1 ()]
*)

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

(** {2 Chunked selectors} *)

(** You may find {!val:(%>)} useful for chaining selectors together, e.g. [drop 5 %> take 2]
*)

val chunk_again : chunking -> chunked -> chunked
(** [chunk_again chunking f] applies [chunked] selector [f] as a selector*)

val first : chunked -> chunked
(** Takes only first chunk *)

val take : int -> chunked -> chunked
(** Takes n chunks *)

val take_nth : int -> chunked -> chunked
(** Take every nth chunk, specifically [0]th, [n]th, [2n]th, [3n]th, ... *)

val drop : int -> chunked -> chunked
(** Discard n chunks *)

(** {1 Lightweight chunking}*)

(** These are strictly less flexible than chunking functions provided in {!section:Chunking},
    but can be significantly more efficient than the equivalent implemented with [chunk ...]
*)

val first_point : t -> t

val take_points : int -> t -> t

val drop_points : int -> t -> t

(** {1 Infix operators} *)

val ( & ) : t -> t -> t
(** {!val:inter} *)

val ( ||| ) : t -> t -> t
(** {!val:union} *)

val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition, mainly for chunked selectors

    [f1 %> f2] is equivalent to [fun x -> x |> f1 |> f2].
*)

(** {1 Resolution} *)

val resolve :
  ?search_using_tz:Time_zone.t -> t -> (interval Seq.t, string) result
(** Resolves a Timere object into a concrete interval sequence *)

(** {1 Pretty printers} *)

val string_of_timestamp :
  ?display_using_tz:Time_zone.t -> ?format:string -> timestamp -> string
(** Pretty printing for timestamp.

    Follows same format string rules and default format string as {!val:Date_time.to_string}.
*)

val pp_timestamp :
  ?display_using_tz:Time_zone.t ->
  ?format:string ->
  unit ->
  Format.formatter ->
  timestamp ->
  unit

val string_of_interval :
  ?display_using_tz:Time_zone.t -> ?format:string -> interval -> string
(** Pretty printing for interval.

    Default format string:
    {v
[{syear} {smon:Xxx} {smday:0X} {shour:0X}:{smin:0X}:{ssec:0X} \
{stzoff-sign}{stzoff-hour:0X}:{stzoff-min:0X}:{stzoff-sec:0X}, {eyear} \
{emon:Xxx} {emday:0X} {ehour:0X}:{emin:0X}:{esec:0X} \
{etzoff-sign}{etzoff-hour:0X}:{etzoff-min:0X}:{etzoff-sec:0X})
    v}

    Follows same format string rules as {!val:Date_time.to_string}, but tags are prefixed with 's' for "start time", and 'e' for "end exc time",
    e.g. for interval [(x, y)]

    - [{syear}] gives year of the [x]
    - [{ehour:cX}] gives hour of the [y]
*)

val pp_interval :
  ?display_using_tz:Time_zone.t ->
  ?format:string ->
  unit ->
  Format.formatter ->
  interval ->
  unit

val pp_intervals :
  ?display_using_tz:Time_zone.t ->
  ?format:string ->
  ?sep:unit Fmt.t ->
  unit ->
  Format.formatter ->
  interval Seq.t ->
  unit

(** {1 S-expressions} *)

(** These functions are suitable for debugging, serializing and deserializing timeres.

    The sexp is a precise description of the steps used to construct a timere.
    As such deserialization is accurate and goes through the exact same construction steps (including validation)
    as one would using the construction API directly.
*)

val to_sexp : t -> CCSexp.t

val pp_sexp : Format.formatter -> t -> unit

val to_sexp_string : t -> string

val of_sexp : CCSexp.t -> (t, string) result

val of_sexp_string : string -> (t, string) result

(** {1 Misc} *)

module Utils : sig
  (** {1 Range flattening} *)

  val flatten_month_ranges : month range Seq.t -> month Seq.t option

  val flatten_month_day_ranges : int range Seq.t -> int Seq.t option

  val flatten_weekday_ranges : weekday range Seq.t -> weekday Seq.t option

  val flatten_month_range_list : month range list -> month list option

  val flatten_month_day_range_list : int range list -> int list option

  val flatten_weekday_range_list : weekday range list -> weekday list option

  val human_int_of_month : month -> int

  val tm_int_of_month : month -> int

  val month_of_human_int : int -> month option

  val month_of_tm_int : int -> month option

  val weekday_of_tm_int : int -> weekday option

  val tm_int_of_weekday : weekday -> int
end
