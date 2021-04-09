type t = {
  s : int64;
  ns : int;
}

let zero = {s = 0L; ns = 0}

let ns_count_in_s = 1_000_000_000

let ns_count_in_s_float = float_of_int ns_count_in_s

let check {s = _; ns} = if ns < 0 then invalid_arg "ns is negative"

let normalize {s; ns} =
  let s' = ns / ns_count_in_s in
  let ns' = ns mod ns_count_in_s in
  { s = Int64.add s (Int64.of_int s'); ns = ns'}

let check_and_normalize (x : t) : t =
  check x;
  normalize x

let add x y : t =
  let { s = s_x; ns = ns_x } = check_and_normalize x in
  let { s = s_y; ns = ns_y } = check_and_normalize y in
  let s = Int64.add s_x s_y in
  let ns = ns_x + ns_y in
  normalize {s; ns}

let sub x y : t =
  let { s = s_x; ns = ns_x } = check_and_normalize x in
  let { s = s_y; ns = ns_y } = check_and_normalize y in
  let ns = ns_x - ns_y in
  if ns >= 0 then { s = Int64.sub s_x s_y; ns}
  else
    let s_x = Int64.pred s_x in
    { s = Int64.sub s_x s_y; ns = ns + ns_count_in_s}

let succ x = add x {s = 0L; ns = 1}

let pred x = sub x {s = 0L; ns = 1}

let equal ({s = s_x; ns = ns_x} : t) ({s = s_y; ns = ns_y} : t) =
  s_x = s_y && ns_x = ns_y

let lt ({s = s_x; ns = ns_x} : t) ({s = s_y; ns = ns_y} : t) =
  (* lexicographical order *)
  s_x < s_y || (s_x = s_y && ns_x < ns_y)

let le x y =
  lt x y || equal x y

let gt x y =
  lt y x

let ge x y =
  le y x

let compare (x : t) (y : t) : int =
  if lt x y then -1 else if x = y then 0 else 1

let to_float ({s; ns} : t) : float =
  Int64.to_float s +. (float_of_int ns /. ns_count_in_s_float)

let of_float (x : float) : t =
  let s = Int64.of_float x in
  let frac = x -. Int64.to_float s in
  let ns =
    max 0 (int_of_float (frac *. ns_count_in_s_float))
  in
  {s; ns}

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
