open Timedesc

module Span : sig
  open Span

  val to_sexp : t -> CCSexp.t

  val to_sexp_string : t -> string

  val of_sexp : CCSexp.t -> (t, string) result

  val of_sexp_string : string -> (t, string) result

  val pp_sexp : Format.formatter -> t -> unit
end

module Date : sig
  open Date

  val to_sexp : t -> CCSexp.t

  val to_sexp_string : t -> string

  val of_sexp : CCSexp.t -> (t, string) result

  val of_sexp_string : string -> (t, string) result

  val pp_sexp : Format.formatter -> t -> unit
end

module Time : sig
  open Time

  val to_sexp : t -> CCSexp.t

  val to_sexp_string : t -> string

  val of_sexp : CCSexp.t -> (t, string) result

  val of_sexp_string : string -> (t, string) result

  val pp_sexp : Format.formatter -> t -> unit
end

module Time_zone : sig
  open Time_zone

  val to_sexp : t -> CCSexp.t

  val of_sexp : CCSexp.t -> t option

  val of_string : string -> t option

  module Db : sig
    open Db

    val of_sexp : CCSexp.t -> db option

    val to_sexp : db -> CCSexp.t

    val of_string : string -> db option
  end
end

val to_sexp : t -> CCSexp.t

val to_sexp_string : t -> string

val of_sexp : CCSexp.t -> (t, string) result

val of_sexp_string : string -> (t, string) result

val pp_sexp : Format.formatter -> t -> unit

module Timestamp : sig
  open Timestamp

  val of_sexp : CCSexp.t -> (t, string) result

  val to_sexp : t -> CCSexp.t
end

module Zoneless : sig
  open Zoneless

  val to_sexp : zoneless -> CCSexp.t

  val of_sexp : CCSexp.t -> (zoneless, string) result
end

module Time_zone_info : sig
  open Time_zone_info

  val of_sexp : CCSexp.t -> (t, string) result

  val to_sexp : t -> CCSexp.t
end
