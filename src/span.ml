type t = {
  s : int64;
  ns : int;
}

let zero = { s = 0L; ns = 0 }

let ns_count_in_s = 1_000_000_000

let ns_count_in_s_float = float_of_int ns_count_in_s

let normalize { s; ns } =
  if ns >= 0 then
    let s_to_add = ns / ns_count_in_s in
    let ns' = ns mod ns_count_in_s in
    { s = Int64.add s (Int64.of_int s_to_add); ns = ns' }
  else
    let ns = -ns in
    let s_to_sub = (ns + ns_count_in_s - 1) / ns_count_in_s in
    let ns_to_sub_from_one_sec = ns mod ns_count_in_s in
    {
      s = Int64.sub s (Int64.of_int s_to_sub);
      ns = ns_count_in_s - ns_to_sub_from_one_sec;
    }

let make ?(s = 0L) ?(ns = 0) () = normalize { s; ns }

let add { s = s_x; ns = ns_x } { s = s_y; ns = ns_y } : t =
  let s = Int64.add s_x s_y in
  let ns = ns_x + ns_y in
  normalize { s; ns }

let sub { s = s_x; ns = ns_x } { s = s_y; ns = ns_y } : t =
  let ns = ns_x - ns_y in
  if ns >= 0 then { s = Int64.sub s_x s_y; ns }
  else
    let s_x = Int64.pred s_x in
    { s = Int64.sub s_x s_y; ns = ns + ns_count_in_s }

let succ x = add x { s = 0L; ns = 1 }

let pred x = sub x { s = 0L; ns = 1 }

let neg { s; ns } =
  if ns = 0 then { s = Int64.neg s; ns }
  else { s = Int64.pred @@ Int64.neg s; ns = ns_count_in_s - ns }

let equal ({ s = s_x; ns = ns_x } : t) ({ s = s_y; ns = ns_y } : t) =
  s_x = s_y && ns_x = ns_y

let neq x y = not (equal x y)

let lt ({ s = s_x; ns = ns_x } : t) ({ s = s_y; ns = ns_y } : t) =
  (* lexicographical order *)
  s_x < s_y || (s_x = s_y && ns_x < ns_y)

let le x y = lt x y || equal x y

let gt x y = lt y x

let ge x y = le y x

let compare (x : t) (y : t) : int =
  if lt x y then -1 else if x = y then 0 else 1

let to_float ({ s; ns } : t) : float =
  Int64.to_float s +. (float_of_int ns /. ns_count_in_s_float)

let of_float (x : float) : t =
  let s = Int64.of_float x in
  let frac = x -. Int64.to_float s in
  let frac = if frac < 0.0 then 1.0 -. frac else frac in
  assert (frac <= 1.0);
  let ns = max 0 (int_of_float (frac *. ns_count_in_s_float)) in
  if x >= 0.0 then normalize { s; ns }
  else normalize { s = Int64.pred s; ns = ns_count_in_s - ns }

let max x y = if ge x y then x else y

let min x y = if le x y then x else y

let ( < ) = lt

let ( <= ) = le

let ( > ) = gt

let ( >= ) = ge

let ( = ) = equal

let ( <> ) = neq

let ( - ) = sub

let ( + ) = add

let abs x = if x >= zero then x else neg x
