open Int64_utils

let ptime_of_timestamp x =
  let d, s =
    if x >= 0L then
      ( x /^ Constants.seconds_in_day |> Int64.to_int,
        Int64.rem (Int64.abs x) Constants.seconds_in_day )
    else
      let x = Int64.abs x in
      let s = Int64.rem x Constants.seconds_in_day in
      ( -1
         * ((x +^ (Constants.seconds_in_day -^ 1L))
            /^ Constants.seconds_in_day
            |> Int64.to_int),
        if s = 0L then s else Constants.seconds_in_day -^ s )
  in
  let ps = s *^ Constants.s_to_ps_mult in
  match Ptime.Span.of_d_ps (d, ps) with
  | None -> Error ()
  | Some span -> (
      match Ptime.of_span span with
      | None -> Error ()
      | Some x -> Ok x
    )

let timestamp_of_ptime x =
  let d, ps = x |> Ptime.to_span |> Ptime.Span.to_d_ps in
  let s = ps /^ Constants.s_to_ps_mult in
  ((Int64.of_int d *^ Constants.seconds_in_day) +^ s)
