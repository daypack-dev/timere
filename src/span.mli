type t = private {
  s : int64;
  ns : int;
}

val ns_count_in_s : int

val ns_count_in_s_float : float

val zero : t

val make : ?s:int64 -> ?ns:int -> unit -> t

val make_small : ?s:int -> ?ns:int -> unit -> t

val add : t -> t -> t

val sub : t -> t -> t

val succ : t -> t

val pred : t -> t

val neg : t -> t

val abs : t -> t

val equal : t -> t -> bool

val lt : t -> t -> bool

val le : t -> t -> bool

val gt : t -> t -> bool

val ge : t -> t -> bool

val compare : t -> t -> int

val to_float : t -> float

val of_float : float -> t

val max : t -> t -> t

val min : t -> t -> t

val ( < ) : t -> t -> bool

val ( <= ) : t -> t -> bool

val ( > ) : t -> t -> bool

val ( >= ) : t -> t -> bool

val ( = ) : t -> t -> bool

val ( <> ) : t -> t -> bool

val ( - ) : t -> t -> t

val ( + ) : t -> t -> t
