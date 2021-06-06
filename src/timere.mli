(** Date time reasoning

    Timere provides a set of highly expressive APIs to describe
    scheduling constraints, and an efficient resolution algorithm
*)

type timestamp = Timedesc.timestamp

type t
(** This is the core type of Timere that represents sets of points in time, more precisely,
    unions of time intervals. For example, "all Mondays of year 2000 at the UTC timezone".

    We call [Timere.t] values "timere object"; internally they are rich
    expressions representing the time computations (union, intersection,
    etc.), lazily forced into more low-level descriptions (lazy sequences
    of intervals).
*)

type inc_exc =
  [ `Inc
  | `Exc
  ]

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

val before : Timedesc.t -> t

val since : Timedesc.t -> t

val after : Timedesc.t -> t

val date_time : Timedesc.t -> t

val date_times : Timedesc.t list -> t

val date_time_seq : Timedesc.t Seq.t -> t

val sorted_date_times : Timedesc.t list -> t

val sorted_date_time_seq : Timedesc.t Seq.t -> t

exception Invalid_timestamp

val timestamp : Timedesc.timestamp -> t

val before_timestamp : Timedesc.timestamp -> t

val since_timestamp : Timedesc.timestamp -> t

val after_timestamp : Timedesc.timestamp -> t

val timestamps : ?skip_invalid:bool -> Timedesc.timestamp list -> t
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

(** {1 Pattern matching constructors} *)

val pattern :
  ?years:int list ->
  ?year_ranges:int range list ->
  ?months:int list ->
  ?month_ranges:int range list ->
  ?days:int list ->
  ?day_ranges:int range list ->
  ?weekdays:Timedesc.weekday list ->
  ?weekday_ranges:Timedesc.weekday range list ->
  ?hours:int list ->
  ?hour_ranges:int range list ->
  ?minutes:int list ->
  ?minute_ranges:int range list ->
  ?seconds:int list ->
  ?second_ranges:int range list ->
  ?ns:int list ->
  ?ns_ranges:int range list ->
  unit ->
  t
(** Pattern matches over date times.

    A pattern [p] matches date time [dt] if
    {v
(year of dt is in p.years or p.year_ranges)
&& (month of dt is in p.months or p.month_ranges)
&& (month day of dt is in p.month_days or p.month_day_ranges)
&& (weekday of dt is in p.weekdays or p.weekday_ranges)
&& (hour of dt is in p.hours or p.hour_ranges)
&& (minute of dt is in p.minutes or p.minute_ranges)
&& (second of dt is in p.seconds or p.second_ranges)
&& (ns of dt is in p.ns or p.ns_ranges)
    v}

    Empty pattern levels are treated as wildcard, e.g. if [p.years] and [p.year_ranges] are both empty,
    then [(dt.year is in p.years or p.year_ranges)] is [true].
*)

val years : int list -> t
(** [years l] is a shorthand for [pattern ~years:l ()] *)

val year_ranges : int range list -> t
(** [year_ranges l] is a shorthand for [pattern ~year_ranges:l ()] *)

val months : int list -> t
(** [months l] is a shorthand for [pattern ~months:l ()] *)

val month_ranges : int range list -> t
(** [month_ranges l] is a shorthand for [pattern ~month_ranges:l ()] *)

val days : int list -> t
(** [days l] is a shorthand for [pattern ~month_days:l ()] *)

val day_ranges : int range list -> t
(** [day_ranges l] is a shorthand for [pattern ~month_day_ranges:l ()] *)

val weekdays : Timedesc.weekday list -> t
(** [weekdays l] is a shorthand for [pattern ~weekdays:l ()] *)

val weekday_ranges : Timedesc.weekday range list -> t
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

val ns : int list -> t
(** [ns l] is a shorthand for [pattern ~ns:l ()] *)

val ns_ranges : int range list -> t
(** [ns_ranges l] is a shorthand for [pattern ~ns_ranges:l ()] *)

val nth_weekday_of_month : int -> Timedesc.weekday -> t
(** [nth_weekday_of_month n wday] picks the nth weekday of all months, where [1 <= n && n <= 5]

    @raise Invalid_argument if [n] is out of range
*)

(** {1 Intervals} *)

(** {2 Explicit intervals} *)

exception Interval_is_invalid

exception Intervals_are_not_sorted

val intervals : ?skip_invalid:bool -> Timedesc.Interval.t list -> t
(** [intervals l]

    [skip_invalid] defaults to [false]

    @raise Interval_is_invalid if [not skip_invalid] and [l] contains an invalid interval
*)

val interval_seq : ?skip_invalid:bool -> Timedesc.Interval.t Seq.t -> t
(** [interval_seq s]

    [skip_invalid] defaults to [false]

    @raise Interval_is_invalid if [not skip_invalid] and [s] contains an invalid interval
*)

val sorted_intervals : ?skip_invalid:bool -> Timedesc.Interval.t list -> t
(** [sorted_intervals l]

    [skip_invalid] defaults to [false]

    @raise Interval_is_invalid if [not skip_invalid] and [l] contains an invalid interval
    @raise Intervals_are_not_sorted if [l] is not sorted
*)

val sorted_interval_seq : ?skip_invalid:bool -> Timedesc.Interval.t Seq.t -> t
(** [sorted_interval_seq s]

    [skip_invalid] defaults to [false]

    @raise Interval_is_invalid if [not skip_invalid] and [s] contains an invalid interval
    @raise Intervals_are_not_sorted if [s] is not sorted
*)

(** {2 Pattern matching intervals} *)

(** Pattern matching intervals are designed to handle intervals where start and end points follow some pattern, but cannot be captured by [pattern] efficiently,
    e.g. you cannot represent "5:30pm to 6:11pm" via a single {!pattern}
*)

module Points : sig
  type t

  type error =
    [ `Invalid_year of int
    | `Invalid_day of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_ns of int
    | `Invalid_pattern_combination
    | `Invalid_tz_info of string option * Timedesc.Span.t
    ]

  exception Error_exn of error

  type lean_toward =
    [ `Earlier
    | `Later
    ]

  val make :
    ?tz:Timedesc.Time_zone.t ->
    ?offset_from_utc:Timedesc.Span.t ->
    ?year:int ->
    ?month:int ->
    ?day:int ->
    ?weekday:Timedesc.weekday ->
    ?hour:int ->
    ?minute:int ->
    ?second:int ->
    ?ns:int ->
    lean_toward:lean_toward ->
    unit ->
    (t, error) result
  (** Call must be exactly one of the following forms
   *  (ignoring [tz], [offset_from_utc] and [lean_toward] which are optional in all cases)
      {[
        make ~year:_                                                       ~lean_toward ()
        make ~year:_ ~month:_                                              ~lean_toward ()
        make ~year:_ ~month:_ ~day:_                                       ~lean_toward ()
        make ~year:_ ~month:_ ~day:_     ~hour:_                           ~lean_toward ()
        make ~year:_ ~month:_ ~day:_     ~hour:_ ~minute:_                 ~lean_toward ()
        make ~year:_ ~month:_ ~day:_     ~hour:_ ~minute:_ ~second:_       ~lean_toward ()
        make ~year:_ ~month:_ ~day:_     ~hour:_ ~minute:_ ~second:_ ~ns:_ ~lean_toward ()

        make         ~month:_                                              ~lean_toward ()
        make         ~month:_ ~day:_                                       ~lean_toward ()
        make         ~month:_ ~day:_     ~hour:_                           ~lean_toward ()
        make         ~month:_ ~day:_     ~hour:_ ~minute:_                 ~lean_toward ()
        make         ~month:_ ~day:_     ~hour:_ ~minute:_ ~second:_       ~lean_toward ()
        make         ~month:_ ~day:_     ~hour:_ ~minute:_ ~second:_ ~ns:_ ~lean_toward ()

        make                  ~day:_                                       ~lean_toward ()
        make                  ~day:_     ~hour:_                           ~lean_toward ()
        make                  ~day:_     ~hour:_ ~minute:_                 ~lean_toward ()
        make                  ~day:_     ~hour:_ ~minute:_ ~second:_       ~lean_toward ()
        make                  ~day:_     ~hour:_ ~minute:_ ~second:_ ~ns:_ ~lean_toward ()

        make                  ~weekday:_                                   ~lean_toward ()
        make                  ~weekday:_ ~hour:_                           ~lean_toward ()
        make                  ~weekday:_ ~hour:_ ~minute:_                 ~lean_toward ()
        make                  ~weekday:_ ~hour:_ ~minute:_ ~second:_       ~lean_toward ()
        make                  ~weekday:_ ~hour:_ ~minute:_ ~second:_ ~ns:_ ~lean_toward ()

        make                             ~hour:_                           ~lean_toward ()
        make                             ~hour:_ ~minute:_                 ~lean_toward ()
        make                             ~hour:_ ~minute:_ ~second:_       ~lean_toward ()
        make                             ~hour:_ ~minute:_ ~second:_ ~ns:_ ~lean_toward ()

        make                                     ~minute:_                 ~lean_toward ()
        make                                     ~minute:_ ~second:_       ~lean_toward ()
        make                                     ~minute:_ ~second:_ ~ns:_ ~lean_toward ()

        make                                               ~second:_       ~lean_toward ()
        make                                               ~second:_ ~ns:_ ~lean_toward ()

        make                                                         ~ns:_ ~lean_toward ()
      ]}

      returns [Error] otherwise
  *)

  val make_exn :
    ?tz:Timedesc.Time_zone.t ->
    ?offset_from_utc:Timedesc.Span.t ->
    ?year:int ->
    ?month:int ->
    ?day:int ->
    ?weekday:Timedesc.weekday ->
    ?hour:int ->
    ?minute:int ->
    ?second:int ->
    ?ns:int ->
    lean_toward:lean_toward ->
    unit ->
    t
  (** @raise Error_exn if [make] fails *)
end

type points = Points.t

val pattern_intervals :
  ?inc_exc:inc_exc ->
  ?bound:Timedesc.Span.t ->
  [ `Whole | `Fst | `Snd ] ->
  points ->
  points ->
  t
(** [pattern_intervals mode p1 p2] for each point [x] matched by [p1],
    then for the earliest point [y] matched by [p2] such that [x < y && y - x <= bound]
    - if [mode = `Whole && inc_exc = `Exc], yields (x, y)
    - if [mode = `Whole && inc_exc = `Inc], yields (x, y + 1)
    - if [mode = `Fst], yields (x, x + 1)
    - if [mode = `Snd], yields (y, y + 1)

    above implies [inc_exc] does not impact operations if mode is [`Fst] or [`Snd].

    [inc_exc] defaults to [`Exc].

    Default [bound] is inferred as follows, and should suffice in yielding desired results for most cases:
    {v
if p2 is YMDHMS then (year of p2 - year of p1 + 1) * 366 days
if p2 is  MDHMS then 366 days
if p2 is   DHMS then
  if day of p1 < day of p2 then 31 - day of p2 days
  else                                      31 days
if p2 is    HMS then  30 hours
if p2 is     MS then   1 hours
if p2 is      S then   1 minutes
    v}
    where we say [p2 is YMDHMS] if [p2 = Points.make_exn ~year:_ ... ()]
    and so on.

    Examples:

    {[
      pattern_intervals `Whole
        (Points.make ~hour:13 ~minute:0 ~second:0 ()) (* p1 *)
        (Points.make ~hour:14 ~minute:0 ~second:0 ()) (* p2 *)
    ]}
    yields all the "1pm to 2pm" intervals, since at each "1pm" mark represented by [p1],
    searching forward up to 24 hour period, we can find a "2pm" mark in [p2]

    {[
      pattern_intervals `Whole
        (Points.make ~month:2 ~day:10 ~hour:13 ~minute:0 ~second:0 ()) (* p1 *)
        (Points.make                  ~hour:14 ~minute:0 ~second:0 ()) (* p2 *)
    ]}
    yields all the "Feb 10th 1pm to 2pm" intervals (or specifically "Feb 10th 1pm to Feb 10th 2pm")

    {[
      pattern_intervals `Whole
        (Points.make ~month:`Feb ~day:10 ~hour:23 ~minute:0 ~second:0 ()) (* p1 *)
        (Points.make                     ~hour:3  ~minute:0 ~second:0 ()) (* p2 *)
    ]}
    yields all the "Feb 10th 11pm to 3am" intervals (or specifically "Feb 10th 11pm to Feb 11th 3am")

    @raise Invalid_argument if bound is negative

    @raise Invalid_argument if precision (number of date time arguments passed to [make_points] during construction)
    of [p1] < precision of [p2]

    For example, [Points.make_exn ~hour:3 ~minute:0 ~second:0 ()]
    has a lower precision than [make_points_exn ~day:10 ~hour:12 ~minute:30 ~second:0 ()].
*)

(** {2 Hour minute second intervals} *)

(** Convenience wrappers around [points] and [pattern_intervals] *)

val hms_intervals : ?inc_exc:inc_exc -> Timedesc.Time.t -> Timedesc.Time.t -> t
(** Same as [pattern_intervals ...] with bound fixed to [Span.For_human.make ~days:1 ()]

    [inc_exc] defaults to [`Exc]
*)

(** {1 Algebraic operations} *)

val inter : t list -> t
(** Intersection of list of timeres.

    [inter []] is equivalent to [always].
*)

val union : t list -> t
(** Union of list of timeres.

    [union []] is equivalent to [empty].
*)

val not : t -> t
(** Negation of timere.

    [not t] is equivalent to all the intervals not included in [t].
*)

val shift : Timedesc.Span.t -> t -> t

val lengthen : Timedesc.Span.t -> t -> t
(** @raise Invalid_argument if duration is negative *)

val with_tz : Timedesc.Time_zone.t -> t -> t
(** [with_tz tz t] changes the time zone to evaluate [t] in to [tz] *)

(** {2 Chunking} *)

type chunked

type chunking =
  [ `Disjoint_intervals
  | `By_duration of Timedesc.Span.t
  | `By_duration_drop_partial of Timedesc.Span.t
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
(** [chunk chunking f t] applies [chunked] selector [f] on [t].

    @raise Invalid_argument if duration is negative in [`By_duration]
    or [`By_duration_drop_partial]
*)

(** {3 Chunked selectors} *)

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

(** {1 Infix operators} *)

val ( &&& ) : t -> t -> t
(** {!val:inter} *)

val ( ||| ) : t -> t -> t
(** {!val:union} *)

val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition, mainly for chunked selectors

    [f1 %> f2] is equivalent to [fun x -> x |> f1 |> f2].
*)

(** {1 Resolution} *)

val resolve :
  ?search_using_tz:Timedesc.Time_zone.t ->
  t ->
  (Timedesc.Interval.t Seq.t, string) result
(** Resolves a Timere object into a concrete interval sequence.
 *
 * Intervals are left-closed, right-open, i.e. each interval is
 * of the form [\[x, y)], which includes x and excludes y.
 * *)

exception Resolution_error of string

val resolve_exn :
  ?search_using_tz:Timedesc.Time_zone.t -> t -> Timedesc.Interval.t Seq.t
(** @raise Resolution_error if [resolve] fails *)

(** {2 S-expressions} *)

(** These functions are suitable for debugging, serializing and deserializing timeres.

    The sexp is a precise description of the steps used to construct a timere.
    As such deserialization is accurate and goes through the exact same construction steps (including validation)
    as one would using the construction API directly.
*)

val to_sexp : t -> CCSexp.t

val to_sexp_string : t -> string

val of_sexp : CCSexp.t -> (t, string) result

val of_sexp_string : string -> (t, string) result

val pp_sexp : Format.formatter -> t -> unit

(** {1 Misc} *)

module Utils : sig
  (** {1 Range flattening} *)

  val flatten_month_ranges : int range Seq.t -> int Seq.t option

  val flatten_month_day_ranges : int range Seq.t -> int Seq.t option

  val flatten_weekday_ranges :
    Timedesc.weekday range Seq.t -> Timedesc.weekday Seq.t option

  val flatten_month_range_list : int range list -> int list option

  val flatten_month_day_range_list : int range list -> int list option

  val flatten_weekday_range_list :
    Timedesc.weekday range list -> Timedesc.weekday list option
end
