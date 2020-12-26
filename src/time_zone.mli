type t

type entry = private {
  is_dst : bool;
  offset : int;
}

type 'a local_result =
  [ `None
  | `Exact of 'a
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

val recorded_offsets : t -> Int_set.t
