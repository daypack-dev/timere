include Time_zone_constants
open Int64_utils

let min_timestamp =
  let x = Ptime.min |> Ptime_utils.timestamp_of_ptime in
  Int64.add x (Int64.of_int greatest_neg_tz_offset_s)

let max_timestamp =
  let x = Ptime.max |> Ptime_utils.timestamp_of_ptime |> Int64.pred in
  Int64.sub x (Int64.of_int greatest_pos_tz_offset_s)

let min_year = 0

let max_year = 9999
