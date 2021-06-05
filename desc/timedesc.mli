(** Time description and manipulations

    Timedesc provides utilities to describe points of time, and properly
    handle calendar and time zone information.
*)

(** {1 Tutorial}

    {2 Getting started}

    Suppose we want to get the time right now, we can simply do [Timedesc.now ()].
    But what if we want to get the time right now in a different time zone? Say New York?
    Then we can simply do:

    [Timedesc.now ~tz_of_date_time:(Timedesc.Time_zone.make_exn "America/New_York") ()].

    And if we want to construct a date time from scratch, we can use constructors such as {!make},
    with similar time zone specification:

    [Timedesc.make ~tz:(Timedesc.Time_zone.make_exn "Australia/Sydney") ~year:2021 ~month:5 ~day:30 ~hour:14 ~minute:10 ~second:0 ()].

    Since we deal with timestamps quite frequently, lets have a look at how Timedesc also makes working with
    them easier.
    Suppose we receive a timestamp similar to the result returned by [Unix.gettimeofday], i.e.
    seconds since unix epoch in [float], we can digest it in myriad ways. If we just
    want to construct a date time out of it, then we can use {!of_timestamp_float_s}.
    If we want to get it into the representation used in Timedesc, say to perform arithmetic operations over it etc, then we can use {!Timestamp.of_float_s}. But in either case, we can always swap back and forth via
    {!to_timestamp} and {!of_timestamp}.

    In general it is better to use {!timestamp} as much as possible, unless you require a precision higher
    than nanosecond. This is because floating point is a lossy representation -
    if you convert a date time to floating point
    and back, you may not get the same date time back (i.e. it may not round trip).
    Also, performing arithmetic operations over floating points can introduce more and more errors, and
    it is advisable to use the arithmetic functions provided in {!Span} or {!Timestamp}.

    To access the values of date time, we can use the constructors such as {!year}, {!month}, {!day}, {!hour}.

    {2:tute_time_zone Time zone}

    By now, one nicety should be obvious: you don't have to worry about what is the time zone offset at when and where - Timedesc
    takes care of that for you properly! All you have to do is to make a time zone following the *nix naming convention.
    However, even though we follow the same naming convention, we don't actually rely on the OS time zone database, and our code
    will run fine on any platform.

    To see what time zones Timedesc supports during run time, we can refer to {!Time_zone.available_time_zones}.
    Alternatively, for a text file containing all the supported time zones by default, refer to {{:https://github.com/daypack-dev/timere/blob/main/gen-artifacts/available-time-zones.txt} [available-time-zones.txt]} in the repository.

    If you are aware of DST: Yes, Timedesc takes care of that for you properly as well - Timedesc does not
    allow you to construct a date time that does not exist for the particular time zone, and any
    ambiguity is made explicit as return type via {!local_result}.

    This does mean Timedesc does not "resolve" the result into one of the possibilities arbitrarily,
    and you need to resolve the ambiguity yourself.
    If such a coercion is desirable, however, then you can use either {!min_of_local_result} or
    {!max_of_local_result}.

    {2 Span/duration}

    Timedesc offers both machine-friendly and human-friendly ways of dealing with spans.

    For the machine-friendly side, functions in the top level of {!Span} provide efficient constructions
    and arithmetic operations.

    For the human-friendly side, {!Span.For_human} provides functions which work at a level closer to
    human language. For instance, we say things like "2 hours and 15 minutes" quite frequently,
    to represent this as {!Span.t}, we can do:

    [Timedesc.Span.For_human.make_exn ~hours:2 ~minutes:15 ()]

    And in the case of fractional descriptions, such as "1.5 hours", we can do:

    [Timedesc.Span.For_human.make_frac_exn ~hours:1.5 ()]

    Finally, to access the human friendly "view", we can use {!Span.For_human.view}.

    {2 Using both Ptime and Timedesc}

    Ptime is a (very) commonly used package in projects due to being very portable, and robust.
    However, it lacks certain features which Timedesc provides, such as
    first class support for time zones, support for different date systems. As such one may wish
    to use both Ptime and Timedesc, especially if Ptime is already being used for a particular project.

    To facilitate such use of both Ptime and Timedesc, utilities for
    converting to and from Ptime types are available as:

    - {!Utils.ptime_span_of_span}
    - {!Utils.ptime_of_timestamp}
    - {!Utils.span_of_ptime_span}
    - {!Utils.timestamp_of_ptime}

    Note that Timedesc only supports nanosecond precision, while Ptime supports picosecond precision.
    If subnanosecond precision is a concern for you, then the above functions are not suitable.

    {1 Advanced usage}

    {2 Unambiguous date time}

    Occasionally, we receive date times which carry both the time zone and the exact offset from UTC.
    Naturally we can discard the time zone since the offset alone suffices in deducing the precise
    timestamp. However, we can actually ask Timedesc to digest both via {!make_unambiguous}, which
    checks the offset against the time zone record to make sure it is actually a possible offset.

    {2 Other calendar systems}

    Other than Gregorian calendar, Timedesc also supports ISO week date and ISO ordinal date.

    To construct date time in the alternative systems, we can use constructors such as
    {!ISO_week_date_time.make} and {!ISO_ord_date_time.make}.

    Then to access the representation in the alternative date systems, we can use accessors
    such as {!iso_week_year}, {!iso_week}, and {!day_of_year}.

    {2 Using date by itself}

    Sometimes we are only interested in the date component rather than both date and time.
    We can use {!Date} module in this case.

    To construct a Gregorian calendar date, we can use {!Date.Ymd_date.make}. To construct
    ISO week date and ISO ordinal date, we can use {!Date.ISO_week_date.make} and {!Date.ISO_ord_date}
    respectively.

    We have similar set of accessors for accessing values of {!Date.t}, such as {!Date.year},
    {!Date.iso_week_year}, {!Date.day_of_year}.

    To obtain a "view" (in a manner similar to the human-friendly "view" from {!Span.For_human}), we can
    use {!Date.ISO_week_date.view} and {!Date.ISO_ord_date.view}.

    {1 Further reading}

    {2 Misconceptions}

    - Time zone offsets are always in hours
    - What we typically consider a time zone, e.g. "Europe/Paris", always has a constant offset
    - With a time zone and a specific date time, we can always obtain a unique "unix timestamp" (time since unix epoch)
    - We can always calculate time zone offset at some date time, and apply it universally for any other date time in the same time zone
    - Many more on various online resources...

    {2 Time zone, time zone offset, and date time}

    It is tempting to think that a time zone maps cleanly to a constant offset, and
    indeed we may define time zone as such, e.g. UTC+1, UTC-10, but this is far from what we mean
    in everyday context.

    Very often, what we consider to be time zone actually represents a table which records what offset to use
    in which period, which we index/refer to by geographical names like "Europe/Paris", "Australia/Sydney".
    These tables are defined by governmental bodies, and attributes of the table, such as offset of
    any particular period, start and end of any particular period, may not show any observable pattern.

    Thus it is not uncommon to see date time errors arising from attempts of applying some formulas universally,
    which might work well for a lot of cases in contemporary time periods, but fail for some combinations.

    We make explicit of above explanation by considering "Europe/Paris" as an example, which observes
    a common form of transition called Daylight Saving Time (DST).

    When DST starts (usually in March), the clocks "jump forward" by 1 hour,
    usually jumping from 2am to 3am, leading 2am to 3am (exclusive) to
    become non-existent.

    Indeed we can observe the lack of continuity of Europe/Paris timeline below (UTC timeline is always continuous):

    {v
                         Mar
UTC          -------------|-------------
                         1am

Europe/Paris -------------|-------------
                       2am 3am
                      (+1) (+2)
v}

    Paris time zone offset also changes from UTC+1 to UTC+2.

    When DST ends (usually in Oct), clocks "jump backward" by 1 hour, usually jumping from 3am to 2am, leading to
    2am to 3am (exclusive) becoming duplicated:

    {v
                         Oct
UTC          -------------|-------------
                         1am

Europe/Paris -------------|-------------
                       3am 2am
                      (+2) (+1)
v}

    Paris time zone offset also changes from UTC+2 to UTC+1.

    Another way of looking at above is when DST is in effect, Paris observes UTC+2, and UTC+1 otherwise:

    {v
                          |-------------DST on------------|
             |---DST off--|                               |---DST off--|

                         Mar                             Oct
UTC          -------------|------------- ... -------------|-------------
                         1am                             1am

Europe/Paris -------------|------------- ... -------------|-------------
                       2am 3am                         3am 2am
                      (+1) (+2)                       (+2) (+1)
v}

    This start and end of the DST on and off periods, along with the corresponding offsets,
    form the basis of the table we mentioned above.

    {2 Timedesc date time API behaviour highlights}

    We highlight some critical cases in practice, and how Timedesc behaves and how it may differ from other libraries.

    Take year 2021 for example, DST starts on 2021 Mar 28 for Paris, causing clocks to jump from 2am to 3am. Pick any
    intermediate point, say 2:30am, we yield an undefined date time. In this case, Timedesc refuses the construction
    of such {!t} in {!make} etc, while some libraries coerce the result into 3:30am.

    And DST ends on 2021 Oct 31,
    causing clocks to jump from 3am to 2am. Say we pick 2:30am again, we are actually pointing at {e two} time points (there are two 2:30am)
    unless we make an explicit selection between the first or second occurance.
    Whenever ambiguity of this form is a possiblity for the result of a function, say {!to_timestamp},
    Timedesc uses {!local_result} variant type, of which [`Single _] indicates lack of ambiguity for the particular result,
    and [`Ambiguous _] indicates the result is ambiguous.

    Some other libraries coerce the ambiguous result
    into one of the two possible choices (which exact one may not be guaranteed). If user wishes to do similar coercions,
    they may use {!min_of_local_result} or {!max_of_local_result}.

    For constructions, {!make} yields a possibly ambiguous construction,
    while {!make_unambiguous} yields an unambiguous construction.
    In general, if you are provided with the exact offset to UTC,
    then [make_unambiguous] is the better choice.
*)

(** {1 Basic exceptions} *)

exception Invalid_format_string of string
(** Printing exception *)

(** {1 Basic types} *)

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

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

val min_of_local_result : 'a local_result -> 'a
(** For [min_of_local_result x]
    - if [x = `Single a], yields [a],
    - if [x = `Ambiguous (a, b)],  yields [a],
*)

val max_of_local_result : 'a local_result -> 'a
(** For [max_of_local_result x]
    - if [x = `Single a], yields [a],
    - if [x = `Ambiguous (a, b)], yields [b],
*)

val equal_local_result :
  eq:('a -> 'a -> bool) -> 'a local_result -> 'a local_result -> bool

(** {1 Span} *)

module Span : sig
  type t
  (** Signed/directional span of time with nanosecond precision *)

  exception Out_of_range

  (** {1 Constants} *)

  val ns_count_in_s : int

  val ns_count_in_s_float : float

  val zero : t

  (** {1 Constructors} *)

  val make : ?s:int64 -> ?ns:int -> unit -> t
  (** [s] defaults to [0L], [ns] defaults to [0]

      [ns] may be negative, and is normalized during construction

      Interpretation of provided input is still [s + ns], i.e. if you wish to
      represent "negative (1 second and 500 nanosecond)", then the call could look like
      [make ~s:(-1L) ~ns:(-500)]

      @raise Out_of_range if the value cannot be represented even after normalization
  *)

  val make_small : ?s:int -> ?ns:int -> unit -> t
  (** Wrapper around [make] *)

  (** {1 Conversion} *)

  val to_s_ns : t -> int64 * int
  (** Yields pair [(s, ns)] where
      - [s] is the signed second of the span
      - [ns] is the unsigned nanosecond offset

      The actual span represented is defined as [s * 10^9 + ns] in nanosecond
      regardless of the sign of [s].

      [ns] is always [>= 0] and [< 1_000_000_000].
  *)

  val to_float_s : t -> float
  (** Returns span in seconds, fraction represents subsecond span.

      Representation is the same as result from [Unix.gettimeofday].
  *)

  val of_float_s : float -> t
  (** Convert from span in seconds, fraction represents subsecond span

      Representation is the same as result from [Unix.gettimeofday].
  *)

  (** {1 Accessors} *)

  val get_s : t -> int64
  (** Yields signed second of span, same as [fst (to_s_ns _)] *)

  val get_ns_offset : t -> int
  (** Yields the unsigned nanosecond offset, same as [snd (to_s_ns _)] *)

  (** {1 Comparison} *)

  val equal : t -> t -> bool

  val lt : t -> t -> bool

  val le : t -> t -> bool

  val gt : t -> t -> bool

  val ge : t -> t -> bool

  val compare : t -> t -> int

  (** {1 Arithmetic} *)

  val add : t -> t -> t

  val sub : t -> t -> t

  val succ : t -> t

  val pred : t -> t

  val neg : t -> t

  val abs : t -> t

  val max : t -> t -> t

  val min : t -> t -> t

  val ceil : t -> t
  (** Rounds up to nearest second *)

  val floor : t -> t
  (** Rounds down to nearest second *)

  val round : t -> t
  (** Rounds to nearest second

      For [round x]
      - if [x.ns >= 500_000_000], then [round x = ceil x]
      - otherwise [round x = floor x]
  *)

  val ( < ) : t -> t -> bool

  val ( <= ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val ( >= ) : t -> t -> bool

  val ( = ) : t -> t -> bool

  val ( <> ) : t -> t -> bool

  val ( - ) : t -> t -> t

  val ( + ) : t -> t -> t

  (** {1 Pretty printing} *)

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit

  (** {1 Sexp} *)

  val to_sexp : t -> CCSexp.t

  val to_sexp_string : t -> string

  val of_sexp : CCSexp.t -> (t, string) result

  val of_sexp_string : string -> (t, string) result

  val pp_sexp : Format.formatter -> t -> unit

  (** {1 Human friendly APIs} *)
  module For_human : sig
    type sign =
      [ `Pos
      | `Neg
      ]

    type view = private {
      sign : sign;
      days : int;
      hours : int;
      minutes : int;
      seconds : int;
      ns : int;
    }

    type error =
      [ `Invalid_days of int
      | `Invalid_hours of int
      | `Invalid_minutes of int
      | `Invalid_seconds of int
      | `Invalid_ns of int
      ]

    type error_f =
      [ `Invalid_days_f of float
      | `Invalid_hours_f of float
      | `Invalid_minutes_f of float
      | `Invalid_seconds_f of float
      | `Invalid_ns of int
      ]

    exception Error_exn of error

    exception Error_f_exn of error_f

    (** {1 Constructors} *)

    val make :
      ?sign:sign ->
      ?days:int ->
      ?hours:int ->
      ?minutes:int ->
      ?seconds:int ->
      ?ns:int ->
      unit ->
      (t, error) result
    (**
       [sign] defaults to [`Pos].

       Returns [Error] if any of the arguments are negative.

       @raise Out_of_range if the value cannot be represented even after normalization
    *)

    val make_exn :
      ?sign:sign ->
      ?days:int ->
      ?hours:int ->
      ?minutes:int ->
      ?seconds:int ->
      ?ns:int ->
      unit ->
      t
    (** @raise Error_exn if [make] fails *)

    val make_frac :
      ?sign:sign ->
      ?days:float ->
      ?hours:float ->
      ?minutes:float ->
      ?seconds:float ->
      ?ns:int ->
      unit ->
      (t, error_f) result
    (**
       [sign] defaults to [`Pos].

       Returns [Error] if any of the arguments are negative.

       @raise Out_of_range if the value cannot be represented even after normalization
    *)

    val make_frac_exn :
      ?sign:sign ->
      ?days:float ->
      ?hours:float ->
      ?minutes:float ->
      ?seconds:float ->
      ?ns:int ->
      unit ->
      t
    (** @raise Error_exn if [make] fails *)

    (** {1 Viewer} *)

    val view : t -> view

    (** {1 Pretty printing} *)

    val pp : ?format:string -> unit -> Format.formatter -> t -> unit
    (** Pretty printing for span.
     *
     *  Default format string:
        {v
{days-nz: days }{hours-nz:X hours }{mins-nz:X mins }{secs:X}{sec-frac:.X} secs
        v}

        Format string specification:
        {v
        {{                      literal {
        {days:unit}             number of days
                                unit is the string used after the number to denote its unit
        {days-nz:unit}          same as above, but does not display if number is zero

        {hours:cXunit}          number of hour, sub-day
                                character 'c' before 'X' is used for padding
                                (leave out character for no padding)
                                unit is the string used after the number to denote its unit
        {hours-nz:cXunit}       same as above, but does not display if number is zero

        {mins:cXunit}           number of minutes, sub-hour
                                character 'c' before 'X' is used for padding
                                (leave out character for no padding)
                                unit is the string used after the number to denote its unit
        {mins-nz:cXunit}        same as above, but does not display if number is zero

        {secs:cXunit}           number of seconds, sub-minute
                                character 'c' before 'X' is used for padding
                                (leave out character for no padding)
                                unit is the string used after the number to denote its unit
        {secs-nz:cXunit}        same as above, but does not display if number is zero

        {sec-frac:cNXunit}      fraction of second, sub-second
                                N determines the number of digits to take after decimal separator
                                if N is not specified, then the smallest number of digits required
                                after decimal separator for a lossless representation is used
                                character c is used as the decimal separator
                                unit is the string used after the number to denote its unit
        {secs-frac-nz:cNXunit}  same as above, but does not display if nanosecond count is
                                zero
        v}
     * *)

    val to_string : ?format:string -> t -> string
  end
end

type timestamp = Span.t
(** Definition of timestamp throughout the library follows the "seconds since unix epoch" definition
*)

(** {1 Date time components} *)

(** {2 Date}

    Implementation of date in:
    - Gregorian calendar ({!Date.Ymd_date})
    - ISO week date calendar ({!Date.ISO_week_date})
    - ISO ordinal date calendar ({!Date.ISO_ord_date})
*)

module Date : sig
  type t

  val equal : t -> t -> bool

  val year : t -> int

  (** {2 Accessors} *)

  val month : t -> int

  val day : t -> int

  val weekday : t -> weekday

  val iso_week_year : t -> int

  val iso_week : t -> int

  val day_of_year : t -> int

  module Ymd_date : sig
    type view = private {
      year : int;
      month : int;
      day : int;
    }

    type error =
      [ `Does_not_exist
      | `Invalid_year of int
      | `Invalid_month of int
      | `Invalid_day of int
      ]

    exception Error_exn of error

    val make : year:int -> month:int -> day:int -> (t, error) result
    (** Constructs a date in the Gregorian calendar.

        Returns [Error `Invalid_year] if [year < 0 || 9999 < year].

        Returns [Error `Invalid_month] if [month < 1 || 12 < month].

        Returns [Error `Invalid_day] if [day < 1 || day count of month < day].
    *)

    val make_exn : year:int -> month:int -> day:int -> t

    val view : t -> view
  end

  module ISO_week_date : sig
    type view = private {
      iso_week_year : int;
      iso_week : int;
      weekday : weekday;
    }

    type error =
      [ `Does_not_exist
      | `Invalid_iso_week_year of int
      | `Invalid_iso_week of int
      ]

    exception Error_exn of error

    val make :
      iso_week_year:int -> iso_week:int -> weekday:weekday -> (t, error) result
    (** Constructs a date in the ISO week calendar.

        Returns [Error `Invalid_iso_week_year] if [iso_week_year < 0 || 9999 < iso_week_year].

        Returns [Error `Invalid_iso_week] if [iso_week < 1 || week count of iso_week_year < iso_week].
    *)

    val make_exn : iso_week_year:int -> iso_week:int -> weekday:weekday -> t

    val view : t -> view
  end

  module ISO_ord_date : sig
    type view = private {
      year : int;
      day_of_year : int;
    }

    type error =
      [ `Does_not_exist
      | `Invalid_year of int
      | `Invalid_day_of_year of int
      ]

    exception Error_exn of error

    val make : year:int -> day_of_year:int -> (t, error) result
    (** Constructs a date in the ISO ordinal calendar.

        Returns [Error `Invalid_year] if [year < 0 || 9999 < year].

        Returns [Error `Invalid_day_of_year] if [day_of_year < 1 || day count of year < day_of_year].
    *)

    val make_exn : year:int -> day_of_year:int -> t

    val view : t -> view
  end
end

(** {2 Time}

    Implementation of time of day with nanosecond precision
*)

module Time : sig
  type t
  (** Representation of time of day *)

  type view = private {
    hour : int;
    minute : int;
    second : int;
    ns : int;
  }

  type error =
    [ `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_s_frac of float
    | `Invalid_ns of int
    ]

  exception Error_exn of error

  (** {1 Constructors} *)

  val make :
    ?ns:int ->
    ?s_frac:float ->
    hour:int ->
    minute:int ->
    second:int ->
    unit ->
    (t, error) result
  (** Constructs {!t} from specification of the time of day.

      Leap second can be specified by providing 60 for [second].
      Note that leap second informtation is lost upon translation to timestamp(s),
      specifically second 60 is treated as second 59.

      [24:00:00] is treated as [23:59:59.999_999_999].

      Nanosecond used is the addition of [ns] and [s_frac * 10^9].

      Returns [Error `Invalid_hour] if [hour < 0 || 24 < hour].

      Returns [Error `Invalid_hour] if [hour = 24] and [minute <> 0 || second <> 0 || total ns <> 0].

      Returns [Error `Invalid_minute] if [minute < 0 || 59 < minute].

      Returns [Error `Invalid_second] if [second < 0 || 60 < second].

      Returns [Error `Invalid_ns] if [s_frac < 0.0].

      Returns [Error `Invalid_ns] if [ns < 0].

      Returns [Error `Invalid_ns] if [total ns >= 10^9].
  *)

  val make_exn :
    ?ns:int ->
    ?s_frac:float ->
    hour:int ->
    minute:int ->
    second:int ->
    unit ->
    t

  (** {1 Comparison} *)

  val equal : t -> t -> bool

  (** {1 Viewer} *)

  val view : t -> view

  (** {1 Accessors} *)

  val hour : t -> int

  val minute : t -> int

  val second : t -> int

  val ns : t -> int

  val is_leap_second : t -> bool

  (** {1 Conversion} *)

  val to_span : t -> Span.t

  val of_span : Span.t -> t option
end

(** {2 Time zone}

    Implementation of time zone which uses IANA time zone database underneath
*)

module Time_zone : sig
  type t

  val make : string -> t option
  (** Makes a time zone from name.

      Naming follows the convention used in [/usr/share/zoneinfo/posix/] distributed on Linux, e.g. "Australia/Sydney".

      See {!val:available_time_zones} for checking usable time zone names at runtime.

      Alternatively, if you are using [timedesc.tzdb.full] (the default implementation for [timedesc.tzdb]), then you can also see
      {{:https://github.com/daypack-dev/timere/tree/main/gen_artifacts/available-time-zones.txt} [available-time-zones.txt]} for available time zones.

      [make] handles names with "UTC" prefix specially, following holds regardless of DB backend chosen
      {ul
        {li [UTC] is alway interpreted as [utc]}
        {li [UTC+/-offset] is always interpreted as call to {!make_offset_only} with the provided signed offset}
        {li {ul
          {li e.g. "UTC+1:30" is equivalent to [make_offset_only (Span.For_human.make_exn ~hours:1 ~minutes:30 ())]}
          {li [offset] may be single/double digit hour, optionally followed by colon and single/double digit minute}
        }}
      }
  *)

  val make_exn : string -> t
  (** @raise Invalid_argument if [make] fails *)

  val name : t -> string

  val utc : t

  val local : unit -> t option

  val local_exn : unit -> t
  (** @raise Invalid_argument when [local] returns [None] *)

  val equal : t -> t -> bool

  val available_time_zones : string list

  val make_offset_only : Span.t -> t option
  (** This is mainly used for when you only have an offset to work with,
      and you don't need to do any accurate search over time zones.

      One use of this is to create a time zone for [to_string] functions.

      Returns [None] when offset exceeds 24 hours in size.
  *)

  val make_offset_only_exn : Span.t -> t

  val to_fixed_offset_from_utc : t -> Span.t option

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

(** {1 Date time}

    Implementation of time zone aware date time in:
    - Gregorian calendar (top level of current module)
    - ISO week date calendar ({!ISO_week_date_time})
    - ISO ordinal date calendar ({!ISO_ord_date_time})
*)

type t
(** This is the main type, and represents a point in the local timeline
    with respect to the residing time zone. Conceptually a triple of "date", "time" (or "time of day"), and time zone.

    A [t] always maps to at least one point on the UTC timeline, and [make] fails if this is not the case.
    [t] may also map to two points on the UTC timeline in the case of DST and without
    an unambiguous offset, however.

    In the ambiguous case, functions which return [_ local_result] will yield an [`Ambiguous _]
    value, and [`Single _] otherwise.

    [ns] may be [>= 10^9] to represent leap second, but always remains [< 2 * 10^9].

    [s] is always [>= 0] and [< 60], even when second 60 is used during construction.
    In other words, second 60 is represented via [ns] field.
*)

type error =
  [ `Does_not_exist
  | `Invalid_year of int
  | `Invalid_month of int
  | `Invalid_day of int
  | `Invalid_hour of int
  | `Invalid_minute of int
  | `Invalid_second of int
  | `Invalid_s_frac of float
  | `Invalid_ns of int
  | `Invalid_tz_info of string option * Span.t
  ]

exception Error_exn of error

val string_of_error : error -> string

(** {2 Constructors} *)

val make :
  ?tz:Time_zone.t ->
  ?ns:int ->
  ?s_frac:float ->
  year:int ->
  month:int ->
  day:int ->
  hour:int ->
  minute:int ->
  second:int ->
  unit ->
  (t, error) result
(** Constructs a date time providing only a time zone (defaults to local time zone).

    A precise offset is inferred if possible.

    Note that this may yield a ambiguous date time if the time zone has varying offsets,
    causing a local date time to appear twice, e.g. countries with DST.

    See {!val:make_unambiguous} for the more precise construction.

    See {!Date.Ymd_date.make} for error handling of date specification.

    See {!Time.make} for error handling of time of day specification.
*)

val make_exn :
  ?tz:Time_zone.t ->
  ?ns:int ->
  ?s_frac:float ->
  year:int ->
  month:int ->
  day:int ->
  hour:int ->
  minute:int ->
  second:int ->
  unit ->
  t
(** @raise Error_exn if [make] fails *)

val make_unambiguous :
  ?tz:Time_zone.t ->
  ?ns:int ->
  ?s_frac:float ->
  year:int ->
  month:int ->
  day:int ->
  hour:int ->
  minute:int ->
  second:int ->
  offset_from_utc:Span.t ->
  unit ->
  (t, error) result
(** Constructs a date time providing time zone offset (offset from UTC), and optionally a time zone.
    As an example, for "UTC+1", you would give a duration of positive 1 hour for [offset_from_utc].

    Subsecond value of [offset_from_utc] is ignored.

    Nanosecond used is the addition of [ns] and [s_frac * 10^9].

    If a time zone is provided, then [offset_from_utc] is checked against the time zone record,
    and returns [Error `Invalid_tz_info] if [offset_from_utc] is not a possible
    offset for the particular date time in said time zone.

    Otherwise same leap second handling and error handling as [make].
*)

val make_unambiguous_exn :
  ?tz:Time_zone.t ->
  ?ns:int ->
  ?s_frac:float ->
  year:int ->
  month:int ->
  day:int ->
  hour:int ->
  minute:int ->
  second:int ->
  offset_from_utc:Span.t ->
  unit ->
  t
(** @raise Error_exn if [make_umabiguous] fails *)

(** {2 Accessors} *)

val date : t -> Date.t

val ymd_date : t -> Date.Ymd_date.view

val iso_week_date : t -> Date.ISO_week_date.view

val iso_ord_date : t -> Date.ISO_ord_date.view

val year : t -> int

val month : t -> int

val day : t -> int

val weekday : t -> weekday

val iso_week_year : t -> int

val iso_week : t -> int

val day_of_year : t -> int

val time : t -> Time.t

val time_view : t -> Time.view

val hour : t -> int

val minute : t -> int

val second : t -> int

val ns : t -> int

val is_leap_second : t -> bool

val tz : t -> Time_zone.t

val offset_from_utc : t -> Span.t local_result

(** {2 Conversion} *)

val to_timestamp : t -> timestamp local_result
(** [to_timestamp] loses information about leap second
*)

val to_timestamp_single : t -> timestamp
(** @raise Invalid_argument if [to_timestamp] does not yield a [`Single] result *)

val to_timestamp_float_s : t -> float local_result
(** Returns timestamp in seconds, fraction represent *)

val to_timestamp_float_s_single : t -> float
(** @raise Invalid_argument if [to_timestamp_float_s] does not yield a [`Single] result *)

val of_timestamp : ?tz_of_date_time:Time_zone.t -> timestamp -> t option

val of_timestamp_exn : ?tz_of_date_time:Time_zone.t -> timestamp -> t

val of_timestamp_float_s : ?tz_of_date_time:Time_zone.t -> float -> t option

val of_timestamp_float_s_exn : ?tz_of_date_time:Time_zone.t -> float -> t

(** {2 Comparison}*)

val equal : t -> t -> bool

val compare_chrono_min : t -> t -> int
(** Compare based on ordering of [min_of_local_result @@ to_timestamp _]

    {b Warning}: [compare_chrono_min x y = 0] does not imply [equal x y]
*)

val compare_chrono_max : t -> t -> int
(** Compare based on ordering of [max_of_local_result @@ to_timestamp _]

    {b Warning}: [compare_chrono_max x y = 0] does not imply [equal x y]
*)

val compare_struct : t -> t -> int
(** Structural comparison, [compare_struct x y = 0] implies [equal x y]

    Ordering does not correspond to chronological ordering
*)

(** {2 Constants} *)

val min_val : t

val max_val : t

(** {2 Now} *)

val now : ?tz_of_date_time:Time_zone.t -> unit -> t

(** {2 Pretty printing} *)

exception Date_time_cannot_deduce_offset_from_utc of t

val pp : ?format:string -> unit -> Format.formatter -> t -> unit
(**
     Pretty printing for date time.

     Default format string:
   {v
{year} {mon:Xxx} {day:0X} {hour:0X}:{min:0X}:{sec:0X}{sec-frac:.} \
{tzoff-sign}{tzoff-hour:0X}:{tzoff-min:0X}:{tzoff-sec:0X}
     v}

     Format string specification:
   {v
{{               literal {
{year}           year
{mon:Xxx}        abbreviated month name (e.g. Jan), casing of 'x' controls the casing
{mon:Xx*}        full month name (e.g. January), casing of first 'x' controls casing of first letter,
                 casing of second 'x' controls casing of following letters
{mon:cX}         month in number form (e.g. 01) character 'c' before 'X' is used for padding
                 (leave out character for no padding)
{day:cX}         month day (e.g.  1) character 'c' before 'X' is used for padding
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
{ns}             nanosecond
{sec-frac:cN}    fraction of second
                 character c is used as the decimal separator
                 N determines the number of digits to take after decimal separator
                 if N is not specified, then the smallest number of digits required
                 after decimal separator for a lossless representation is used
                 result is truncated to said number of digits
{tzoff-sign}     time zone offset sign ('+' or '-')
                 raises Date_time_cannot_deduce_offset_from_utc if time zone offset cannot be calculated
{tzoff-hour:cX}  time zone offset hour, follows same padding rule as "{hour:cX}"
                 raises Date_time_cannot_deduce_offset_from_utc if time zone offset cannot be calculated
{tzoff-min:cX}   time zone offset minute, follows same padding rule as "{min:cX}"
                 raises Date_time_cannot_deduce_offset_from_utc if time zone offset cannot be calculated
{tzoff-sec:cX}   time zone offset second, follows same padding rule as "{sec:cX}"
                 raises Date_time_cannot_deduce_offset_from_utc if time zone offset cannot be calculated
     v}
*)

val to_string : ?format:string -> t -> string option
(**
     String conversion using [pp].

     Returns [None] instead of raising exception when time zone offset cannot be deduced but required by the format string
*)

val pp_rfc3339 : ?frac_s:int -> unit -> Format.formatter -> t -> unit
(**
     Pretty prints according to RFC3339, e.g. [2020-01-20T13:00:00.0001+10].

     [frac_s] defaults to as many digits as required for a lossless representation.

     @raise Invalid_argument if [frac_s < 0 || frac_s > 9]

     @raise Date_time_cannot_deduce_offset_from_utc if time zone offset cannot be calculated
*)

val pp_rfc3339_milli : Format.formatter -> t -> unit

val pp_rfc3339_micro : Format.formatter -> t -> unit

val pp_rfc3339_nano : Format.formatter -> t -> unit

val to_rfc3339 : ?frac_s:int -> t -> string option
(** String conversion using [pp_rfc3339].

      Returns [None] if time zone offset cannot be deduced instead of raising exception.
*)

val to_rfc3339_milli : t -> string option

val to_rfc3339_micro : t -> string option

val to_rfc3339_nano : t -> string option

(** {2 Parsing} *)

val of_iso8601 : string -> (t, string) result
(**
     Parses a subset of ISO8601, up to 9 fractional digits for second (nanosecond precision).

     If more than 9 fractional digits are provided, then only the first 9 digits are used, i.e. no rounding.
*)

(** {2 Sexp} *)

val to_sexp : t -> CCSexp.t

val to_sexp_string : t -> string

val of_sexp : CCSexp.t -> (t, string) result

val of_sexp_string : string -> (t, string) result

val pp_sexp : Format.formatter -> t -> unit

(** {1 Timestamp} *)

module Timestamp : sig
  (** Timestamp specific functions
  *)

  type t = timestamp

  (** {1 Constants} *)

  val min_val : t

  val max_val : t

  (** {1 Now} *)

  val now : unit -> t

  (** {1 Re-export from Span} *)

  (** {2 Conversion} *)

  val to_s_ns : t -> int64 * int

  val to_float_s : t -> float
  (** Returns span in seconds, fraction represents subsecond span.

      Representation is the same as result from [Unix.gettimeofday].
  *)

  val of_float_s : float -> t
  (** Convert from span in seconds, fraction represents subsecond span

      Representation is the same as result from [Unix.gettimeofday].
  *)

  (** {2 Accessors} *)

  val get_s : t -> int64

  val get_ns_offset : t -> int

  (** {2 Comparison} *)

  val equal : t -> t -> bool

  val lt : t -> t -> bool

  val le : t -> t -> bool

  val gt : t -> t -> bool

  val ge : t -> t -> bool

  val compare : t -> t -> int

  (** {2 Arithmetic} *)

  val add : t -> t -> t

  val sub : t -> t -> t

  val succ : t -> t

  val pred : t -> t

  val neg : t -> t

  val abs : t -> t

  val max : t -> t -> t

  val min : t -> t -> t

  val ceil : t -> t

  val floor : t -> t

  val round : t -> t

  val ( < ) : t -> t -> bool

  val ( <= ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val ( >= ) : t -> t -> bool

  val ( = ) : t -> t -> bool

  val ( <> ) : t -> t -> bool

  val ( - ) : t -> t -> t

  val ( + ) : t -> t -> t

  (** {1 Pretty printing} *)

  val pp :
    ?display_using_tz:Time_zone.t ->
    ?format:string ->
    unit ->
    Format.formatter ->
    t ->
    unit
  (** Pretty printing for timestamp.

      Follows same format string rules and default format string as {!val:pp}.
  *)

  val to_string :
    ?display_using_tz:Time_zone.t -> ?format:string -> timestamp -> string

  val pp_rfc3339 : ?frac_s:int -> unit -> Format.formatter -> t -> unit
  (**
     Pretty prints according to RFC3339, e.g. [2020-01-20T13:00:00.0001+10].

     [frac_s] determines the number of fractional digits to include.

     @raise Invalid_argument if [frac_s < 0 || frac_s > 9]
  *)

  val pp_rfc3339_milli : Format.formatter -> t -> unit

  val pp_rfc3339_micro : Format.formatter -> t -> unit

  val pp_rfc3339_nano : Format.formatter -> t -> unit

  val to_rfc3339 : ?frac_s:int -> t -> string

  val to_rfc3339_milli : t -> string

  val to_rfc3339_micro : t -> string

  val to_rfc3339_nano : t -> string

  (** {1 Parsing} *)

  val of_iso8601 : string -> (t, string) result
  (**
     Parses a subset of ISO8601, up to 9 fractional digits for second (nanosecond precision).

     If more than 9 fractional digits are provided, then only the first 9 digits are used, i.e. no rounding.
  *)

  (** {1 Sexp} *)

  val of_sexp : CCSexp.t -> (t, string) result

  val to_sexp : t -> CCSexp.t
end

(** {1 Interval} *)

module Interval : sig
  type t = timestamp * timestamp
  (** Left-closed, right-open interval, i.e. interval is
   *  of the form [\[x, y)], which includes x and excludes y
  *)

  (** {1 Comparison} *)

  val equal : t -> t -> bool

  val lt : t -> t -> bool

  val le : t -> t -> bool

  val gt : t -> t -> bool

  val ge : t -> t -> bool

  val compare : t -> t -> int

  (** {1 Pretty printing} *)

  val pp :
    ?display_using_tz:Time_zone.t ->
    ?format:string ->
    unit ->
    Format.formatter ->
    t ->
    unit
  (** Pretty printing for interval.

        Default format string:
      {v
[{syear} {smon:Xxx} {sday:0X} {shour:0X}:{smin:0X}:{ssec:0X}{ssec-frac:.} \
{stzoff-sign}{stzoff-hour:0X}:{stzoff-min:0X}:{stzoff-sec:0X}, {eyear} \
{emon:Xxx} {eday:0X} {ehour:0X}:{emin:0X}:{esec:0X}{esec-frac:.} \
{etzoff-sign}{etzoff-hour:0X}:{etzoff-min:0X}:{etzoff-sec:0X})
    v}

        Follows same format string rules as {!val:Date_time.to_string}, but tags are prefixed with 's' for "start time", and 'e' for "end exc time",
        e.g. for interval [(x, y)]

      - [{syear}] gives year of the [x]
      - [{ehour:cX}] gives hour of the [y]
  *)

  val to_string : ?display_using_tz:Time_zone.t -> ?format:string -> t -> string

  val pp_seq :
    ?display_using_tz:Time_zone.t ->
    ?format:string ->
    ?sep:(Format.formatter -> unit -> unit) ->
    unit ->
    Format.formatter ->
    t Seq.t ->
    unit
end

(** {1 Time zone-less date time} *)

module Zoneless : sig
  type zoneless

  type error_when_zoned =
    [ `Does_not_exist
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_when_zoned_exn of error_when_zoned

  (** {1 Constructors} *)

  val make : Date.t -> Time.t -> zoneless

  (** {1 Accessors} *)

  val date : zoneless -> Date.t

  val time : zoneless -> Time.t

  (** {1 Comparison} *)

  val equal : zoneless -> zoneless -> bool

  (** {1 Conversion} *)

  val to_timestamp_local : zoneless -> timestamp
  (** This yields a "local timestamp" - we pretend we are in the UTC time zone, and
      calculate seconds since unix epoch
  *)

  val to_zoned : ?tz:Time_zone.t -> zoneless -> (t, error_when_zoned) result
  (** [tz] defaults to result of {!Utils.get_local_tz_for_arg}
  *)

  val to_zoned_exn : ?tz:Time_zone.t -> zoneless -> t
  (** @raise Error_when_zoned_exn if [to_zoned] fails *)

  val to_zoned_unambiguous :
    ?tz:Time_zone.t ->
    offset_from_utc:Span.t ->
    zoneless ->
    (t, error_when_zoned) result

  val to_zoned_unambiguous_exn :
    ?tz:Time_zone.t -> offset_from_utc:Span.t -> zoneless -> t
  (** @raise Error_when_zoned_exn if [to_zoned_unambiguous] fails *)

  val of_zoned : t -> zoneless

  (** {1 Parsing} *)

  val of_iso8601 : string -> (zoneless, string) result
  (**
       Parses a subset of ISO8601, up to 9 fractional digits for second (nanosecond precision).

       If more than 9 fractional digits are provided, then only the first 9 digits are used, i.e. no rounding.
  *)

  val maybe_zoneless_of_iso8601 :
    string -> ([ `Zoned of t | `Zoneless of zoneless ], string) result

  (** {1 Sexp} *)

  val to_sexp : zoneless -> CCSexp.t

  val of_sexp : CCSexp.t -> (zoneless, string) result
end

(** {1 Other date time systems}*)

module ISO_week_date_time : sig
  type error =
    [ `Does_not_exist
    | `Invalid_iso_week_year of int
    | `Invalid_iso_week of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_s_frac of float
    | `Invalid_ns of int
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_exn of error

  val make :
    ?tz:Time_zone.t ->
    ?ns:int ->
    ?s_frac:float ->
    iso_week_year:int ->
    iso_week:int ->
    weekday:weekday ->
    hour:int ->
    minute:int ->
    second:int ->
    unit ->
    (t, error) result

  val make_exn :
    ?tz:Time_zone.t ->
    ?ns:int ->
    ?s_frac:float ->
    iso_week_year:int ->
    iso_week:int ->
    weekday:weekday ->
    hour:int ->
    minute:int ->
    second:int ->
    unit ->
    t

  val make_unambiguous :
    ?tz:Time_zone.t ->
    ?ns:int ->
    ?s_frac:float ->
    iso_week_year:int ->
    iso_week:int ->
    weekday:weekday ->
    hour:int ->
    minute:int ->
    second:int ->
    offset_from_utc:Span.t ->
    unit ->
    (t, error) result

  val make_unambiguous_exn :
    ?tz:Time_zone.t ->
    ?ns:int ->
    ?s_frac:float ->
    iso_week_year:int ->
    iso_week:int ->
    weekday:weekday ->
    hour:int ->
    minute:int ->
    second:int ->
    offset_from_utc:Span.t ->
    unit ->
    t
end

module ISO_ord_date_time : sig
  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_day_of_year of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_s_frac of float
    | `Invalid_ns of int
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_exn of error

  val make :
    ?tz:Time_zone.t ->
    ?ns:int ->
    ?s_frac:float ->
    year:int ->
    day_of_year:int ->
    hour:int ->
    minute:int ->
    second:int ->
    unit ->
    (t, error) result

  val make_exn :
    ?tz:Time_zone.t ->
    ?ns:int ->
    ?s_frac:float ->
    year:int ->
    day_of_year:int ->
    hour:int ->
    minute:int ->
    second:int ->
    unit ->
    t

  val make_unambiguous :
    ?tz:Time_zone.t ->
    ?ns:int ->
    ?s_frac:float ->
    year:int ->
    day_of_year:int ->
    hour:int ->
    minute:int ->
    second:int ->
    offset_from_utc:Span.t ->
    unit ->
    (t, error) result

  val make_unambiguous_exn :
    ?tz:Time_zone.t ->
    ?ns:int ->
    ?s_frac:float ->
    year:int ->
    day_of_year:int ->
    hour:int ->
    minute:int ->
    second:int ->
    offset_from_utc:Span.t ->
    unit ->
    t
end

(** {1 Misc} *)

module Time_zone_info : sig
  (** Time zone information that can be attached to date time like data
  *)

  type t
  (** Time zone information of date time.

  *)

  type error =
    [ `Missing_both_tz_and_fixed_offset_from_utc
    | `Invalid_offset of Span.t
    | `Unrecorded_offset of Span.t
    ]

  val make :
    ?tz:Time_zone.t ->
    ?fixed_offset_from_utc:Span.t ->
    unit ->
    (t, error) result
  (**
   *  [tz] is the time zone tied. If only an offset provided during construction, then one a fixed offset one is constructed, e.g.
   *  if say only offset of 10 hours is provided, [tz] becomes "UTC+10".
   *
   *  [fixed_offset_from_utc] is the fixed offset from UTC.
   *  If an offset is not provided but the time zone can be represented by a fixed offset, then said offset is used,
   *  e.g. "UTC+1" can be represented by fixed offset of 1 hour.
   * *)

  val tz : t -> Time_zone.t

  val fixed_offset_from_utc : t -> Span.t option

  val equal : t -> t -> bool

  val of_sexp : CCSexp.t -> (t, string) result

  val to_sexp : t -> CCSexp.t
end

module Utils : sig
  val ptime_span_of_span : Span.t -> Ptime.span option

  val ptime_of_timestamp : timestamp -> Ptime.t option

  val span_of_ptime_span : Ptime.span -> Span.t
  (** {b Warning}: Subnanosecond information is lost in this conversion
  *)

  val timestamp_of_ptime : Ptime.t -> timestamp
  (** {b Warning}: Subnanosecond information is lost in this conversion
  *)

  val day_count_of_year : year:int -> int

  val day_count_of_month : year:int -> month:int -> int

  val week_count_of_iso_week_year : iso_week_year:int -> int

  (** {1 Month utils} *)

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

  val human_int_of_month : month -> int
  (** This yields the usual human readable numbering of 1 to 12 inclusive *)

  val index_of_month : month -> int
  (** This yields the index based numbering of 0 to 11 inclusive *)

  val month_of_human_int : int -> month option

  val month_of_index : int -> month option

  val weekday_of_tm_int : int -> weekday option

  val tm_int_of_weekday : weekday -> int

  val weekday_of_iso_int : int -> weekday option

  val iso_int_of_weekday : weekday -> int

  val get_local_tz_for_arg : unit -> Time_zone.t

  val abbr_string_of_weekday : weekday -> string

  val is_leap_year : year:int -> bool

  val jd_of_ymd : year:int -> month:int -> day:int -> int

  val jd_of_date : Date.t -> int

  val jd_of_unix_epoch : int

  val jd_span_of_unix_epoch : Span.t
end
