type t = {
  s : int64;
  ns : int;
}

let zero = { s = 0L; ns = 0 }

let ns_count_in_s = 1_000_000_000

let ns_count_in_s_float = float_of_int ns_count_in_s

let normalize { s; ns } =
  if ns >= 0 then
    let s_to_add = ns / ns_count_in_s in
    let ns' = ns mod ns_count_in_s in
    { s = Int64.add s (Int64.of_int s_to_add); ns = ns' }
  else
    let ns = -ns in
    let s_to_sub = (ns + ns_count_in_s - 1) / ns_count_in_s in
    let ns_to_sub_from_one_s = ns mod ns_count_in_s in
    {
      s = Int64.sub s (Int64.of_int s_to_sub);
      ns = ns_count_in_s - ns_to_sub_from_one_s;
    }

let make ?(s = 0L) ?(ns = 0) () = normalize { s; ns }

let make_small ?(s = 0) ?ns () = make ~s:(Int64.of_int s) ?ns ()

let add { s = s_x; ns = ns_x } { s = s_y; ns = ns_y } : t =
  let s = Int64.add s_x s_y in
  let ns = ns_x + ns_y in
  normalize { s; ns }

let sub { s = s_x; ns = ns_x } { s = s_y; ns = ns_y } : t =
  let ns = ns_x - ns_y in
  if ns >= 0 then { s = Int64.sub s_x s_y; ns }
  else
    let s_x = Int64.pred s_x in
    { s = Int64.sub s_x s_y; ns = ns + ns_count_in_s }

let succ x = add x { s = 0L; ns = 1 }

let pred x = sub x { s = 0L; ns = 1 }

let neg { s; ns } =
  if ns = 0 then { s = Int64.neg s; ns }
  else { s = Int64.pred @@ Int64.neg s; ns = ns_count_in_s - ns }

let equal ({ s = s_x; ns = ns_x } : t) ({ s = s_y; ns = ns_y } : t) =
  s_x = s_y && ns_x = ns_y

let neq x y = not (equal x y)

let lt ({ s = s_x; ns = ns_x } : t) ({ s = s_y; ns = ns_y } : t) =
  (* lexicographic order *)
  s_x < s_y || (s_x = s_y && ns_x < ns_y)

let le x y = lt x y || equal x y

let gt x y = lt y x

let ge x y = le y x

let abs x = if ge x zero then x else neg x

let compare (x : t) (y : t) : int =
  if lt x y then -1 else if equal x y then 0 else 1

let to_float_s ({ s; ns } : t) : float =
  Int64.to_float s +. (float_of_int ns /. ns_count_in_s_float)

let of_float_s (x : float) : t =
  let s = Int64.of_float x in
  let frac = CCFloat.abs (x -. Int64.to_float s) in
  assert (frac <= 1.0);
  let ns = max 0 (int_of_float (frac *. ns_count_in_s_float)) in
  normalize
    (if x >= 0.0 then { s; ns }
     else { s = Int64.pred s; ns = ns_count_in_s - ns })

let max x y = if ge x y then x else y

let min x y = if le x y then x else y

let ceil x = if x.ns = 0 then x else { s = Int64.succ x.s; ns = 0 }

let floor x = { x with ns = 0 }

let round x =
  if x.ns >= ns_count_in_s / 2 then { s = Int64.succ x.s; ns = 0 }
  else { x with ns = 0 }

module For_human' = struct
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

  type view = {
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

  let view (x : t) : view =
    let sign = if lt x zero then `Neg else `Pos in
    let { s; ns } = abs x in
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

  let to_span (t : view) : t =
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
    let x = make ~s ~ns:t.ns () in
    match t.sign with `Pos -> x | `Neg -> neg x

  let span_of_raw (r : raw) : t =
    let span =
      add
        (of_float_s
           ((r.days *. Float_multipliers.day_to_seconds)
            +. (r.hours *. Float_multipliers.hour_to_seconds)
            +. (r.minutes *. Float_multipliers.minute_to_seconds)
            +. r.seconds))
        (make ~ns:r.ns ())
    in
    match r.sign with `Pos -> span | `Neg -> neg span

  let make ?(sign = `Pos) ?(days = 0) ?(hours = 0) ?(minutes = 0) ?(seconds = 0)
      ?(ns = 0) () : (t, error) result =
    if days < 0 then Error (`Invalid_days days)
    else if hours < 0 then Error (`Invalid_hours hours)
    else if minutes < 0 then Error (`Invalid_minutes minutes)
    else if seconds < 0 then Error (`Invalid_seconds seconds)
    else if ns < 0 then Error (`Invalid_ns ns)
    else Ok (({ sign; days; hours; minutes; seconds; ns } : view) |> to_span)

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
      |> CCResult.return

  let make_frac_exn ?sign ?days ?hours ?minutes ?seconds ?ns () =
    match make_frac ?sign ?days ?hours ?minutes ?seconds ?ns () with
    | Ok x -> x
    | Error e -> raise (Error_f_exn e)
end

let ( < ) = lt

let ( <= ) = le

let ( > ) = gt

let ( >= ) = ge

let ( = ) = equal

let ( <> ) = neq

let ( - ) = sub

let ( + ) = add
