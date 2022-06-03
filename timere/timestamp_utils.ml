let rec timestamp_safe_sub a b =
  let open Timedesc.Span in
  if b >= zero then
    if a - Timedesc.Timestamp.min_val >= b then a - b
    else Timedesc.Timestamp.min_val
  else
    let b' = abs b in
    timestamp_safe_add a b'

and timestamp_safe_add a b =
  let open Timedesc.Span in
  if b >= zero then
    if Timedesc.Timestamp.max_val - a >= b then a + b
    else Timedesc.Timestamp.max_val
  else
    let b' = abs b in
    timestamp_safe_sub a b'
