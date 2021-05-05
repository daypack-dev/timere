type t = private {
  s : int64;
  ns : int;
}

module For_human' : sig
  type sign =
    [ `Pos
    | `Neg
    ]

  type view = private {
    sign : sign;
    days : int;
    hours : int;
    minutes : int;
    seconds : int;
    ns : int;
  }

  type error =
    [ `Invalid_days of int
    | `Invalid_hours of int
    | `Invalid_minutes of int
    | `Invalid_seconds of int
    | `Invalid_ns of int
    ]

  type error_f =
    [ `Invalid_days_f of float
    | `Invalid_hours_f of float
    | `Invalid_minutes_f of float
    | `Invalid_seconds_f of float
    | `Invalid_ns of int
    ]

  exception Error_exn of error

  exception Error_f_exn of error_f

  val make :
    ?sign:sign ->
    ?days:int ->
    ?hours:int ->
    ?minutes:int ->
    ?seconds:int ->
    ?ns:int ->
    unit ->
    (t, error) result

  val make_exn :
    ?sign:sign ->
    ?days:int ->
    ?hours:int ->
    ?minutes:int ->
    ?seconds:int ->
    ?ns:int ->
    unit ->
    t

  val make_frac :
    ?sign:sign ->
    ?days:float ->
    ?hours:float ->
    ?minutes:float ->
    ?seconds:float ->
    ?ns:int ->
    unit ->
    (t, error_f) result

  val make_frac_exn :
    ?sign:sign ->
    ?days:float ->
    ?hours:float ->
    ?minutes:float ->
    ?seconds:float ->
    ?ns:int ->
    unit ->
    t

  val view : t -> view
end

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

val to_float_s : t -> float

val of_float_s : float -> t

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
