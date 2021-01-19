include Time_zone_constants

let min_timestamp =
  let x = Ptime.min |> Ptime.to_float_s |> Int64.of_float in
  Int64.add x (Int64.of_int greatest_neg_tz_offset_s)

let max_timestamp =
  let x = Ptime.max |> Ptime.to_float_s |> Int64.of_float |> Int64.pred in
  Int64.sub x (Int64.of_int greatest_pos_tz_offset_s)

let min_year = 0

let max_year = 9999
