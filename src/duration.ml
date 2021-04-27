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
    ?(ns = 0) () : (t, error) result =
  if days < 0 then Error (`Invalid_days days)
  else if hours < 0 then Error (`Invalid_hours hours)
  else if minutes < 0 then Error (`Invalid_minutes minutes)
  else if seconds < 0 then Error (`Invalid_seconds seconds)
  else if ns < 0 then Error (`Invalid_ns ns)
  else Ok (({ sign; days; hours; minutes; seconds; ns } : t) |> normalize)

let make_exn ?sign ?days ?hours ?minutes ?seconds ?ns () =
  match make ?sign ?days ?hours ?minutes ?seconds ?ns () with
  | Ok x -> x
  | Error e -> raise (Error_exn e)

let make_frac ?(sign = `Pos) ?(days = 0.0) ?(hours = 0.0) ?(minutes = 0.0)
    ?(seconds = 0.0) ?(ns = 0) () : (t, error_f) result =
  if days < 0.0 then Error (`Invalid_days_f days)
  else if hours < 0.0 then Error (`Invalid_hours_f hours)
  else if minutes < 0.0 then Error (`Invalid_minutes_f minutes)
  else if seconds < 0.0 then Error (`Invalid_seconds_f seconds)
  else if ns < 0 then Error (`Invalid_ns ns)
  else
    ({ sign; days; hours; minutes; seconds; ns } : raw)
    |> span_of_raw
    |> of_span
    |> CCResult.return

let make_frac_exn ?sign ?days ?hours ?minutes ?seconds ?ns () =
  match make_frac ?sign ?days ?hours ?minutes ?seconds ?ns () with
  | Ok x -> x
  | Error e -> raise (Error_f_exn e)
