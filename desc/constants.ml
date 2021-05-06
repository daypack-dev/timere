include Time_zone_constants

let timestamp_min : Span.t =
  let x = Ptime.min |> Ptime_utils.timestamp_of_ptime in
  Span.(x + make ~s:(Int64.of_int greatest_neg_tz_offset_s) ())

let timestamp_max : Span.t =
  let x = Ptime.max |> Ptime_utils.timestamp_of_ptime in
  Span.(
    x - make_small ~s:1 () - make ~s:(Int64.of_int greatest_pos_tz_offset_s) ())

let min_year = 0

let max_year = 9999

let one_day = Span.For_human'.make_exn ~days:1 ()
