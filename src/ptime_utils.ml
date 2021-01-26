open Int64_utils

let s_to_ps_mult = 1_000_000_000_000L

let seconds_in_day = 24L *^ 60L *^ 60L

let ptime_of_timestamp x =
  let d, s =
    if x >= 0L then
      ( x /^ seconds_in_day |> Int64.to_int,
        Int64.rem (Int64.abs x) seconds_in_day )
    else
      let x = Int64.abs x in
      let s = Int64.rem x seconds_in_day in
      ( -1 * ((x +^ (seconds_in_day -^ 1L)) /^ seconds_in_day |> Int64.to_int),
        if s = 0L then s else seconds_in_day -^ s )
  in
  let ps = s *^ s_to_ps_mult in
  match Ptime.Span.of_d_ps (d, ps) with
  | None -> None
  | Some span -> Ptime.of_span span

let timestamp_of_ptime x =
  let d, ps = x |> Ptime.to_span |> Ptime.Span.to_d_ps in
  let s = ps /^ s_to_ps_mult in
  (Int64.of_int d *^ seconds_in_day) +^ s
