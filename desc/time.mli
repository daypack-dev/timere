type t = private {
  hour : int;
  minute : int;
  second : int;
  ns : int;
}

type error =
  [ `Invalid_hour of int
  | `Invalid_minute of int
  | `Invalid_second of int
  | `Invalid_s_frac of float
  | `Invalid_ns of int
  ]

exception Error_exn of error

val make :
  ?ns:int ->
  ?s_frac:float ->
  hour:int ->
  minute:int ->
  second:int ->
  unit ->
  (t, error) result

val make_exn :
  ?ns:int -> ?s_frac:float -> hour:int -> minute:int -> second:int -> unit -> t

val to_span : t -> Span.t

val of_span : Span.t -> t option

val is_leap_second : t -> bool

val equal : t -> t -> bool
