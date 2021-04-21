module Int64_multipliers = struct
  let minute_to_seconds = 60L

  let hour_to_seconds = Int64.mul 60L minute_to_seconds

  let day_to_seconds = Int64.mul 24L hour_to_seconds
end

module Float_multipliers = struct
  let minute_to_seconds = Int64.to_float Int64_multipliers.minute_to_seconds

  let hour_to_seconds = Int64.to_float Int64_multipliers.hour_to_seconds

  let day_to_seconds = Int64.to_float Int64_multipliers.day_to_seconds
end

type sign =
  [ `Pos
  | `Neg
  ]

type raw = {
  sign : sign;
  days : float;
  hours : float;
  minutes : float;
  seconds : float;
  ns : int;
}

type t = {
  sign : sign;
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
  ns : int;
}

let is_pos (x : t) = x.sign = `Pos

let is_neg (x : t) = x.sign = `Neg

let equal (x : t) (y : t) =
  x.sign = y.sign
  && x.days = y.days
  && x.hours = y.hours
  && x.minutes = y.minutes
  && x.seconds = y.seconds
  && x.ns = x.ns

let zero : t =
  { sign = `Pos; days = 0; hours = 0; minutes = 0; seconds = 0; ns = 0 }

let of_span (x : Span.t) : t =
  let sign = if Span.(x < zero) then `Neg else `Pos in
  let { Span.s; ns } = Span.abs x in
  let seconds = Int64.rem s 60L in
  let minutes = Int64.div s 60L in
  let hours = Int64.div minutes 60L in
  let days = Int64.div hours 24L in
  let hours = Int64.rem hours 24L in
  let minutes = Int64.rem minutes 60L in
  {
    sign;
    days = Int64.to_int days;
    hours = Int64.to_int hours;
    minutes = Int64.to_int minutes;
    seconds = Int64.to_int seconds;
    ns;
  }

let to_span (t : t) : Span.t =
  let open Int64_utils in
  let days = Int64.of_int t.days in
  let hours = Int64.of_int t.hours in
  let minutes = Int64.of_int t.minutes in
  let seconds = Int64.of_int t.seconds in
  let s =
    (days *^ Int64_multipliers.day_to_seconds)
    +^ (hours *^ Int64_multipliers.hour_to_seconds)
    +^ (minutes *^ Int64_multipliers.minute_to_seconds)
    +^ seconds
  in
  let x = Span.make ~s ~ns:t.ns () in
  match t.sign with `Pos -> x | `Neg -> Span.neg x

let span_of_raw (r : raw) : Span.t =
  let span =
    Span.(
      of_float
        ((r.days *. Float_multipliers.day_to_seconds)
         +. (r.hours *. Float_multipliers.hour_to_seconds)
         +. (r.minutes *. Float_multipliers.minute_to_seconds)
         +. r.seconds)
      + make ~ns:r.ns ())
  in
  match r.sign with `Pos -> span | `Neg -> Span.neg span

let normalize (t : t) : t = t |> to_span |> of_span

let make ?(sign = `Pos) ?(days = 0) ?(hours = 0) ?(minutes = 0) ?(seconds = 0)
    ?(ns = 0) () : t =
  if days >= 0 && hours >= 0 && minutes >= 0 && seconds >= 0 then
    ({ sign; days; hours; minutes; seconds; ns } : t) |> normalize
  else invalid_arg "make"

let make_frac ?(sign = `Pos) ?(days = 0.0) ?(hours = 0.0) ?(minutes = 0.0)
    ?(seconds = 0.0) ?(ns = 0) () : t =
  if days >= 0.0 && hours >= 0.0 && minutes >= 0.0 && seconds >= 0.0 && ns >= 0
  then
    ({ sign; days; hours; minutes; seconds; ns } : raw)
    |> span_of_raw
    |> of_span
  else invalid_arg "make_frac"
