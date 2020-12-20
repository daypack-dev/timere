type t

type entry = private {
  is_dst : bool;
  offset : int;
}

type 'a local_result = [
  | `None
  | `Exact of 'a
  | `Ambiguous of 'a * 'a
]

val utc : t

val is_utc : t -> bool

val make : string -> t

val available_time_zones : string list

val lookup_timestamp_utc : t -> int64 -> entry option

val lookup_timestamp_local : t -> int64 -> entry local_result

val transitions : t -> ((int64 * int64) * entry) list
