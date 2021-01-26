include Time_zone_constants
open Int64_utils

let timestamp_min =
  let x = Ptime.min |> Ptime_utils.timestamp_of_ptime in
  x +^ Int64.of_int greatest_neg_tz_offset_s

let timestamp_max =
  let x = Ptime.max |> Ptime_utils.timestamp_of_ptime |> Int64.pred in
  x -^ Int64.of_int greatest_pos_tz_offset_s

let min_year = 0

let max_year = 9999
