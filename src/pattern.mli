type time_pattern = {
  years : int list;
  months : Time.month list;
  month_days : int list;
  weekdays : Time.weekday list;
  hours : int list;
  minutes : int list;
  seconds : int list;
  unix_seconds : int64 list;
}

type time_pattern_error =
  | Invalid_years of int list
  | Invalid_month_days of int list
  | Invalid_hours of int list
  | Invalid_minutes of int list
  | Invalid_seconds of int list
  | Invalid_unix_seconds of int64 list

type error =
  | Invalid_search_param of Search_param.error
  | Invalid_time_pattern of time_pattern_error

type time_range_pattern = time_pattern Range.range

(* type single_or_ranges =
 *   | Single_time_pattern of time_pattern
 *   | Time_range_patterns of time_range_pattern list *)

val empty : time_pattern

module Check : sig
  val check_time_pattern : time_pattern -> (unit, time_pattern_error) result

  val check_time_range_pattern :
    time_range_pattern -> (unit, time_pattern_error) result
end

module Single_pattern : sig
  val matching_date_times :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern ->
    (Time.Date_time.t Seq.t, error) result

  val matching_unix_seconds :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern ->
    (int64 Seq.t, error) result

  val matching_date_time_ranges :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern ->
    (Time.Date_time.t Range.range Seq.t, error) result

  val matching_time_slots :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern ->
    (Time_slot.t Seq.t, error) result

  val matching_time_slots_round_robin_non_decreasing :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern list ->
    (Time_slot.t list Seq.t, error) result

  val matching_time_slots_round_robin_non_decreasing_flat :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern list ->
    (Time_slot.t Seq.t, error) result

  val next_match_date_time :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern ->
    (Time.Date_time.t option, error) result

  val next_match_unix_second :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern ->
    (int64 option, error) result

  val next_match_time_slot :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_pattern ->
    (Time_slot.t option, error) result
end

module Range_pattern : sig
  val matching_time_slots :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_range_pattern ->
    (Time_slot.t Seq.t, error) result

  val next_match_time_slot :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_range_pattern ->
    (Time_slot.t option, error) result

  val matching_time_slots_multi :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_range_pattern list ->
    (Time_slot.t Seq.t, error) result

  val next_match_time_slot_multi :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_range_pattern list ->
    ((int64 * int64) option, error) result

  val matching_time_slots_round_robin_non_decreasing :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_range_pattern list ->
    (Time_slot.t list Seq.t, error) result

  val matching_time_slots_round_robin_non_decreasing_flat :
    allow_search_param_override:bool ->
    Search_param.t ->
    time_range_pattern list ->
    (Time_slot.t Seq.t, error) result
end

(* module Single_or_ranges : sig
 *   val matching_time_slots :
 *     Search_param.t -> single_or_ranges -> (Time_slot.t Seq.t, error) result
 * 
 *   val next_match_time_slot :
 *     Search_param.t -> single_or_ranges -> (Time_slot.t option, error) result
 * 
 *   val matching_time_slots_round_robin_non_decreasing :
 *     Search_param.t -> single_or_ranges -> (Time_slot.t list Seq.t, error) result
 * 
 *   val matching_time_slots_round_robin_non_decreasing_flat :
 *     Search_param.t -> single_or_ranges -> (Time_slot.t Seq.t, error) result
 * end *)

module Equal : sig
  val equal : time_pattern -> time_pattern -> bool
end

module Parsers : sig
  val cron_expr : (time_pattern, unit) MParser.t

  val time_pattern_core_expr : (time_pattern, unit) MParser.t

  val time_pattern_expr : (time_pattern, unit) MParser.t
end

module Of_string : sig
  val time_pattern_of_cron_string : string -> (time_pattern, string) result

  val time_pattern_of_string : string -> (time_pattern, string) result
end

module To_string : sig
  val string_of_error : error -> string

  val debug_string_of_weekdays : Time.weekday list -> string

  val debug_string_of_month_days : int list -> string

  val debug_string_of_time_pattern :
    ?indent_level:int -> ?buffer:Buffer.t -> time_pattern -> string

  val debug_string_of_time_range_pattern :
    ?indent_level:int -> ?buffer:Buffer.t -> time_range_pattern -> string

  (* val debug_string_of_single_or_ranges :
   *   ?indent_level:int -> ?buffer:Buffer.t -> single_or_ranges -> string *)
end

module Print : sig
  val debug_print_time_pattern : ?indent_level:int -> time_pattern -> unit

  val debug_print_time_range_pattern :
    ?indent_level:int -> time_range_pattern -> unit

  (* val debug_print_single_or_ranges :
   *   ?indent_level:int -> single_or_ranges -> unit *)
end

module Serialize : sig
  val pack_pattern : time_pattern -> Time_pattern_t.time_pattern
end

module Deserialize : sig
  val unpack_pattern : Time_pattern_t.time_pattern -> time_pattern
end
