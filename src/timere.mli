module Time : sig
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

  module Date_time : sig
    type t

    val make :
      year:int ->
      month:month ->
      day:int ->
      hour:int ->
      minute:int ->
      second:int ->
      tz_offset_s:int ->
      t
  end

  type t

  val chunk : int64 -> t -> t

  val inter : t -> t -> t

  val union : t -> t -> t

  val not_in : t -> t

  val interval_inc : t -> t -> t

  val interval_exc : t -> t -> t

  val flatten : t Seq.t -> t

  val flatten_list : t list -> t

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
    t

  val of_unix_second_interval : int64 * int64 -> t
end

module Resolver : sig
  module Search_param : sig
    type start =
      [ `Unix_second of int64
      | `Date_time of Time.Date_time.t
      ]

    type t

    type error =
      | Invalid_start
      | Invalid_intervals
      | Invalid_search_years_ahead
      | Too_far_into_future

    val of_intervals :
      ?search_using_tz_offset_s:Time.tz_offset_s ->
      (int64 * int64) list ->
      (t, error) result

    val of_years_ahead :
      ?search_using_tz_offset_s:Time.tz_offset_s ->
      ?start:start ->
      int ->
      (t, error) result
  end
end
