open Timedesc

module Time_zone : sig
  open Time_zone

  val to_json : t -> Yojson.Basic.t

  val of_json : Yojson.Basic.t -> t option

  val of_string : string -> t option
end
