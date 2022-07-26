val to_sexp : Timedesc.t -> Sexplib.Sexp.t

val to_sexp_string : Timedesc.t -> string

val of_sexp : Sexplib.Sexp.t -> (Timedesc.t, string) result

val of_sexp_string : string -> (Timedesc.t, string) result

val pp_sexp : Format.formatter -> Timedesc.t -> unit

module Span : sig
  val to_sexp : Timedesc.Span.t -> Sexplib.Sexp.t

  val to_sexp_string : Timedesc.Span.t -> string

  val of_sexp : Sexplib.Sexp.t -> (Timedesc.Span.t, string) result

  val of_sexp_string : string -> (Timedesc.Span.t, string) result

  val pp_sexp : Format.formatter -> Timedesc.Span.t -> unit
end

module Timestamp : sig
  val of_sexp : Sexplib.Sexp.t -> (Timedesc.Timestamp.t, string) result

  val to_sexp : Timedesc.Timestamp.t -> Sexplib.Sexp.t
end

module Date : sig
  val to_sexp : Timedesc.Date.t -> Sexplib.Sexp.t

  val to_sexp_string : Timedesc.Date.t -> string

  val of_sexp : Sexplib.Sexp.t -> (Timedesc.Date.t, string) result

  val of_sexp_string : string -> (Timedesc.Date.t, string) result

  val pp_sexp : Format.formatter -> Timedesc.Date.t -> unit
end

module Time : sig
  val to_sexp : Timedesc.Time.t -> Sexplib.Sexp.t

  val to_sexp_string : Timedesc.Time.t -> string

  val of_sexp : Sexplib.Sexp.t -> (Timedesc.Time.t, string) result

  val of_sexp_string : string -> (Timedesc.Time.t, string) result

  val pp_sexp : Format.formatter -> Timedesc.Time.t -> unit
end

module Time_zone : sig
  val to_sexp : Timedesc.Time_zone.t -> Sexplib.Sexp.t

  val of_sexp : Sexplib.Sexp.t -> Timedesc.Time_zone.t option

  val of_string : string -> Timedesc.Time_zone.t option

  module Db : sig
    val of_sexp : Sexplib.Sexp.t -> Timedesc.Time_zone.Db.db option

    val to_sexp : Timedesc.Time_zone.Db.db -> Sexplib.Sexp.t

    val of_string : string -> Timedesc.Time_zone.Db.db option
  end
end

module Zoneless : sig
  val to_sexp : Timedesc.Zoneless.zoneless -> Sexplib.Sexp.t

  val of_sexp : Sexplib.Sexp.t -> (Timedesc.Zoneless.zoneless, string) result
end

module Time_zone_info : sig
  val of_sexp : Sexplib.Sexp.t -> (Timedesc.Time_zone_info.t, string) result

  val to_sexp : Timedesc.Time_zone_info.t -> Sexplib.Sexp.t
end
