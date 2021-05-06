open Int64_utils

let s_to_ps_mult = 1_000_000_000_000L

let ns_to_ps_mult = 1_000

let seconds_in_day = 24L *^ 60L *^ 60L

let ptime_span_of_span ({ s; ns } : Span.t) =
  let d, s =
    if s >= 0L then
      ( s /^ seconds_in_day |> Int64.to_int,
        Int64.rem (Int64.abs s) seconds_in_day )
    else
      let x = Int64.abs s in
      let s = Int64.rem x seconds_in_day in
      ( -1 * ((x +^ (seconds_in_day -^ 1L)) /^ seconds_in_day |> Int64.to_int),
        if s = 0L then s else seconds_in_day -^ s )
  in
  let ps = (s *^ s_to_ps_mult) +^ Int64.of_int (ns * ns_to_ps_mult) in
  Ptime.Span.of_d_ps (d, ps)

let ptime_of_timestamp x =
  match ptime_span_of_span x with None -> None | Some x -> Ptime.of_span x

let span_of_ptime_span x =
  let d, ps = x |> Ptime.Span.to_d_ps in
  let s = ps /^ s_to_ps_mult in
  let ns = Int64.to_int (Int64.rem ps s_to_ps_mult) / ns_to_ps_mult in
  Span.make ~s:((Int64.of_int d *^ seconds_in_day) +^ s) ~ns ()

let timestamp_of_ptime x = span_of_ptime_span (Ptime.to_span x)
