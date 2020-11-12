type raw = {
  days : float;
  hours : float;
  minutes : float;
  seconds : int;
}

type t = {
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

let zero : t = { days = 0; hours = 0; minutes = 0; seconds = 0 }

let of_seconds (x : int64) : (t, unit) result =
  if x < 0L then Error ()
  else
    let seconds = Int64.rem x 60L in
    let minutes = Int64.div x 60L in
    let hours = Int64.div minutes 60L in
    let days = Int64.div hours 24L in
    let hours = Int64.rem hours 24L in
    let minutes = Int64.rem minutes 60L in
    Ok
      {
        days = Int64.to_int days;
        hours = Int64.to_int hours;
        minutes = Int64.to_int minutes;
        seconds = Int64.to_int seconds;
      }

let to_seconds (t : t) : int64 =
  let open Int64_utils in
  let days = Int64.of_int t.days in
  let hours = Int64.of_int t.hours in
  let minutes = Int64.of_int t.minutes in
  let seconds = Int64.of_int t.seconds in
  (days *^ Time.Int64_multipliers.day_to_seconds)
  +^ (hours *^ Time.Int64_multipliers.hour_to_seconds)
  +^ (minutes *^ Time.Int64_multipliers.minute_to_seconds)
  +^ seconds

let seconds_of_raw (r : raw) : int64 =
  (r.days *. Time.Float_multipliers.day_to_seconds)
  +. (r.hours *. Time.Float_multipliers.hour_to_seconds)
  +. (r.minutes *. Time.Float_multipliers.minute_to_seconds)
  |> Int64.of_float
  |> Int64.add (Int64.of_int r.seconds)

let normalize (t : t) : t = t |> to_seconds |> of_seconds |> Result.get_ok

let make ~days ~hours ~minutes ~seconds : (t, unit) result =
  if days >= 0 && hours >= 0 && minutes >= 0 && seconds >= 0 then
    Ok (({ days; hours; minutes; seconds } : t) |> normalize)
  else Error ()

let make_frac ~days ~hours ~minutes ~seconds : (t, unit) result =
  if days >= 0.0 && hours >= 0.0 && minutes >= 0.0 && seconds >= 0 then
    Ok
      ( ({ days; hours; minutes; seconds } : raw)
        |> seconds_of_raw
        |> of_seconds
        |> Result.get_ok )
  else Error ()
