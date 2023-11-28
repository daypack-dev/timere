type t

type entry = {
  is_dst : bool;
  offset : int;
}

type 'a local_date_time_result =
  [ `None
  | `Single of 'a
  | `Ambiguous of 'a * 'a
  ]

val equal : t -> t -> bool

val name : t -> string

val to_fixed_offset_from_utc : t -> Span.t option

val utc : t

val local : unit -> t option

val local_exn : unit -> t

val make : string -> t option

val make_exn : string -> t

val recorded_offsets : t -> int list

val available_time_zones : string list

val lookup_timestamp_utc : t -> int64 -> entry option

val lookup_timestamp_local : t -> int64 -> entry local_date_time_result

module Raw : sig
  val of_table : name:string -> Timedesc_tzdb.table -> t option

  val of_table_exn : name:string -> Timedesc_tzdb.table -> t

  val of_transitions : name:string -> (int64 * entry) list -> t option

  val to_transitions : t -> ((int64 * int64) * entry) list

  val to_transition_seq : t -> ((int64 * int64) * entry) Seq.t
end

val offset_is_recorded : Span.t -> t -> bool

val make_offset_only : Span.t -> t option

val make_offset_only_exn : Span.t -> t

module Compressed : sig
  val to_string : t -> string

  val of_string : string -> t option

  val of_string_exn : string -> t
end

module Db : sig
  type db = Timedesc_tzdb.table Timedesc_tzdb.M.t

  val empty : db

  val add : t -> db -> db

  val find_opt : string -> db -> t option

  val remove : string -> db -> db

  val of_seq : t Seq.t -> db

  val add_seq : db -> t Seq.t -> db

  val names : db -> string list

  module Compressed : sig
    val to_string : db -> string

    val half_compressed_of_string : string -> string Timedesc_tzdb.M.t option

    val half_compressed_of_string_exn : string -> string Timedesc_tzdb.M.t

    val of_string : string -> db option

    val of_string_exn : string -> db
  end
end
