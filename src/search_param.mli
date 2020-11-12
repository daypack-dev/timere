type start =
  [ `Unix_second of int64
  | `Date_time of Time.Date_time.t
  ]

type typ =
  | Time_slots of Time_slot.t list
  | Years_ahead of {
      start : start;
      years_ahead : int;
    }

type t = {
  search_using_tz_offset_s : Time.tz_offset_s option;
  typ : typ;
}

type error =
  | Invalid_start
  | Invalid_time_slots
  | Invalid_search_years_ahead
  | Too_far_into_future

val push_search_param_to_later_start : start:int64 -> t -> (t, unit) result

val start_date_time_and_search_years_ahead_of_search_param :
  t -> (Time.Date_time.t * int) option

module Check : sig
  val check_search_param : t -> (unit, error) result
end

val make_using_time_slots :
  ?search_using_tz_offset_s:int -> Time_slot.t list -> (t, error) result

val make_using_years_ahead :
  ?search_using_tz_offset_s:int -> ?start:start -> int -> (t, error) result
