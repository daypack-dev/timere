let min_timestamp = Ptime.min |> Ptime.to_float_s |> Int64.of_float

let max_timestamp =
  Ptime.max |> Ptime.to_float_s |> Int64.of_float |> Int64.pred
