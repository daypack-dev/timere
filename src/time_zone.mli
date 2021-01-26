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

val make : string -> t option

val make_exn : string -> t

val available_time_zones : string list

val lookup_timestamp_utc : t -> int64 -> entry option

val lookup_timestamp_local : t -> int64 -> entry local_result

module Raw : sig
  val of_transitions : name:string -> (int64 * entry) list -> t option

  val to_transitions : t -> ((int64 * int64) * entry) list

  val to_transition_seq : t -> ((int64 * int64) * entry) Seq.t
end

val offset_is_recorded : int -> t -> bool

val make_offset_only : ?name:string -> int -> t

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
