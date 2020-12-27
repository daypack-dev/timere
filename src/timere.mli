(** {1 Sprintf exception }*)

exception Invalid_format_string of string

type tz_offset_s = int

type timestamp = int64

type 'a range =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

type t

val always : t

val empty : t

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
  ?strict:bool ->
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

type hms = private {
  hour : int;
  minute : int;
  second : int;
}

val make_hms : hour:int -> minute:int -> second:int -> (hms, unit) result

val make_hms_exn : hour:int -> minute:int -> second:int -> hms

(** {1 Time zone} *)

module Time_zone : sig
  type t

  val make : string -> (t, unit) result

  val make_exn : string -> t

  val name : t -> string

  val utc : t

  val available_time_zones : string list
end

(** {1 Time zone change} *)

val with_tz : Time_zone.t -> t -> t

(** {1 Algebraic operations} *)

val inter : t list -> t

val union : t list -> t

val not : t -> t

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

  val make_exn :
    ?days:int -> ?hours:int -> ?minutes:int -> ?seconds:int -> unit -> t

  val make_frac :
    ?days:float ->
    ?hours:float ->
    ?minutes:float ->
    ?seconds:int ->
    unit ->
    (t, unit) result

  val make_frac_exn :
    ?days:float -> ?hours:float -> ?minutes:float -> ?seconds:int -> unit -> t

  val zero : t

  val of_seconds : int64 -> (t, unit) result

  val to_seconds : t -> int64

  val sprint : t -> string

  val pp : Format.formatter -> t -> unit
end

val shift : Duration.t -> t -> t

val lengthen : Duration.t -> t -> t

(** {1 Discrete time points} *)

val cur_timestamp : unit -> int64

val min_timestamp : int64

val max_timestamp : int64

exception Invalid_timestamp

val of_timestamps : ?skip_invalid:bool -> timestamp list -> t

val of_timestamp_seq : ?skip_invalid:bool -> timestamp Seq.t -> t

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

  val make_exn :
    year:int ->
    month:month ->
    day:int ->
    hour:int ->
    minute:int ->
    second:int ->
    tz:Time_zone.t ->
    t

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

  type 'a local_result =
    [ `None
    | `Exact of 'a
    | `Ambiguous of 'a * 'a
    ]

  val to_timestamp : t -> timestamp local_result

  val to_timestamp_exact : t -> timestamp

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

(** {1 List and Filtering operations} *)

type chunked

type chunking =
  [ `Disjoint_intervals
  | `By_duration of Duration.t
  | `By_duration_drop_partial of Duration.t
  | `At_year_boundary
  | `At_month_boundary
  ]

val chunk : chunking -> (chunked -> chunked) -> t -> t

val chunk_again : chunking -> chunked -> chunked

val first : chunked -> chunked

val take : int -> chunked -> chunked

val take_nth : int -> chunked -> chunked

val drop : int -> chunked -> chunked

val first_point : t -> t

val take_n_points : int -> t -> t

val skip_n_points : int -> t -> t

(** {1 Manual intervals} *)

val interval_dt_inc : Date_time.t -> Date_time.t -> t

val interval_dt_exc : Date_time.t -> Date_time.t -> t

val interval_inc : timestamp -> timestamp -> t

val interval_exc : timestamp -> timestamp -> t

val hms_interval_inc : hms -> hms -> t

val hms_interval_exc : hms -> hms -> t

exception Interval_is_invalid

exception Interval_is_empty

exception Intervals_are_not_sorted

exception Intervals_are_not_disjoint

type interval = timestamp * timestamp

val of_intervals : ?skip_invalid:bool -> interval list -> t

val of_interval_seq : ?skip_invalid:bool -> interval Seq.t -> t

val of_sorted_intervals : ?skip_invalid:bool -> interval list -> t

val of_sorted_interval_seq : ?skip_invalid:bool -> interval Seq.t -> t

val of_hms_intervals : (hms * hms) Seq.t -> t

(** {1 Search oriented operations} *)

val after : Duration.t -> t -> t -> t

val between_inc : Duration.t -> t -> t -> t

val between_exc : Duration.t -> t -> t -> t

(** {1 Infix operators} *)

module Infix : sig
  val ( & ) : t -> t -> t

  val ( ||| ) : t -> t -> t
end

val resolve :
  ?search_using_tz:Time_zone.t -> t -> (interval Seq.t, string) result

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

(** {1 S-expressions} *)

val to_sexp : t -> CCSexp.t

val pp_sexp : Format.formatter -> t -> unit

val to_sexp_string : t -> string

val of_sexp : CCSexp.t -> (t, string) result

val of_sexp_string : string -> (t, string) result

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
