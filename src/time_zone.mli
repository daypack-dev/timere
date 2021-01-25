type t

type entry = private {
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

val transitions : t -> ((int64 * int64) * entry) list

val transition_seq : t -> ((int64 * int64) * entry) Seq.t

val offset_is_recorded : int -> t -> bool

val make_offset_only : ?name:string -> int -> t

val to_sexp : t -> CCSexp.t

val of_sexp : CCSexp.t -> (t, unit) result

val to_sexp_string : t -> string

val of_sexp_string : string -> (t, unit) result

val to_json_string : t -> string

val of_json_string : string -> (t, unit) result
