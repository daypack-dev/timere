type t = int64 * int

let ns_count_in_s = 1_000_000_000

let ns_count_in_s_float = float_of_int ns_count_in_s

let of_timestamp x = (x, 0)

let to_timestamp (x, _) = x

let check (_x, ns) = if ns < 0 then invalid_arg "ns is negative"

let normalize (x, ns) =
  let x' = ns / ns_count_in_s in
  let ns' = ns mod ns_count_in_s in
  (Int64.add x (Int64.of_int x'), ns')

let check_and_normalize (x, ns) : t =
  check (x, ns);
  normalize (x, ns)

let add x y : t =
  let x, ns_x = check_and_normalize x in
  let y, ns_y = check_and_normalize y in
  let ns = ns_x + ns_y in
  normalize (Int64.add x y, ns)

let sub x y : t =
  let x, ns_x = check_and_normalize x in
  let y, ns_y = check_and_normalize y in
  let ns = ns_x - ns_y in
  if ns >= 0 then (Int64.sub x y, ns)
  else
    let x = Int64.pred x in
    (Int64.sub x y, ns + ns_count_in_s)

let succ x = add x (0L, 1)

let pred x = sub x (0L, 1)

let equal ((x, ns_x) : t) ((y, ns_y) : t) =
  x = y && ns_x = ns_y

let lt ((x, ns_x) : t) ((y, ns_y) : t) =
  (* lexicographical order *)
  x < y || (x = y && ns_x < ns_y)

let le x y =
  lt x y || equal x y

let gt x y =
  lt y x

let ge x y =
  le y x

let compare (x : t) (y : t) : int =
  if lt x y then -1 else if x = y then 0 else 1

let to_timestamp_float ((x, ns) : t) : float =
  Int64.to_float x +. (float_of_int ns /. ns_count_in_s_float)

let of_timestamp_float (x : float) : t =
  let s = Int64.of_float x in
  let frac = x -. Int64.to_float s in
  (s, max 0 (int_of_float (frac *. ns_count_in_s_float)))

let max x y =
  if ge x y then
    x
  else
    y

let min x y =
  if le x y then
    x
  else
    y

let (<) = lt

let (<=) = le

let (>) = gt

let (>=) = ge

let (=) = equal

let (-) = sub

let (+) = add
