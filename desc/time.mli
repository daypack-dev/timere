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
  | `Invalid_frac of float
  | `Invalid_ns of int
  ]

val make : hour:int -> minute:int -> second:int -> ns:int -> frac:float -> (t, error) result

val to_span : t -> Span.t
