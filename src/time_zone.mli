type t

type entry = {
  is_dst : bool;
  offset : int;
}

type 'a local_result =
  [ `None
  | `Single of 'a
  | `Ambiguous of 'a * 'a
  ]

val equal : t -> t -> bool

val name : t -> string

val utc : t

val make : string -> (t, unit) result

val make_exn : string -> t

val available_time_zones : string list

val lookup_timestamp_utc : t -> int64 -> entry option

val lookup_timestamp_local : t -> int64 -> entry local_result

module Raw : sig
  val table_of_transitions : (int64 * entry) list -> (Timere_tzdb.table, unit) result
val of_transitions : name:string -> (int64 * entry) list -> (t, unit) result

val to_transitions : t -> ((int64 * int64) * entry) list

val to_transition_seq : t -> ((int64 * int64) * entry) Seq.t
    end

val offset_is_recorded : int -> t -> bool

val make_offset_only : ?name:string -> int -> t

module Sexp : sig
val to_sexp : t -> CCSexp.t

val of_sexp : CCSexp.t -> (t, unit) result

val to_string : t -> string

val of_string : string -> (t, unit) result
    end

module JSON : sig
val to_string : t -> string

val of_string : string -> (t, unit) result
end
