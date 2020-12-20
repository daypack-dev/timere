include Time_zone_data

type t = {
  name : string;
  table : table;
}

type 'a local_result = [
  | `None
  | `Exact of 'a
  | `Ambiguous of 'a * 'a
]

let utc = { name = "UTC"; table = [||] }

let is_utc t = t.name = utc.name

let available_time_zones = String_map.bindings db |> List.map (fun (k, _) -> k)

let make name : t =
  match String_map.find_opt name db with
  | Some table -> { name; table }
  | None -> invalid_arg "make: Invalid time zone name"

let dummy_entry : entry = { is_dst = false; offset = 0 }

let lookup_timestamp_utc (t : t) timestamp =
  if is_utc t then Some { is_dst = false; offset = 0 }
  else
    match
      CCArray.bsearch
        ~cmp:(fun (k1, _) (k2, _) -> Int64.compare k1 k2)
        (timestamp, dummy_entry) t.table
    with
    | `At i -> Some (snd t.table.(i))
    | `All_lower -> None
    | `All_bigger -> None
    | `Just_after i -> Some (snd t.table.(i))
    | `Empty -> None

let local_interval_of_table (table : table) (i : int) =
  let (start_utc, entry) = table.(i) in
  let end_exc_utc =
    if i = (Array.length table) - 1 then
      Ptime.(max |> to_float_s |> Int64.of_float)
    else
      fst table.(i + 1)
  in
  (Int64.add start_utc (Int64.of_int entry.offset),
   Int64.add end_exc_utc (Int64.of_int entry.offset))

let interval_mem ((x, y) : int64 * int64) (t : int64) =
  x <= t && t < y

let lookup_timestamp_local (t : t) timestamp : entry local_result =
  let index =
    match
      CCArray.bsearch
        ~cmp:(fun (k1, _) (k2, _) -> Int64.compare k1 k2)
        (timestamp, dummy_entry) t.table
    with
    | `At i -> Some i
    | `All_lower -> None
    | `All_bigger -> None
    | `Just_after i -> Some i
    | `Empty -> None
  in
  match index with
  | None -> `None
  | Some index ->
    let x1 =
      if index > 0 && interval_mem (local_interval_of_table t.table (index - 1)) timestamp then
        Some (snd t.table.(index - 1))
      else
        None
    in
    let x2 =
      if interval_mem (local_interval_of_table t.table index) timestamp then
        Some (snd t.table.(index))
      else
        None
    in
    let x3 =
      if index < Array.length t.table - 1 && interval_mem (local_interval_of_table t.table (index + 1)) timestamp then
        Some (snd (t.table.(index + 1)))
      else
        None
    in
    match x1, x2, x3 with
    | None, None, None -> `None
    | Some x, None, None
    | None, Some x, None
    | None, None, Some x -> `Exact x
    | Some x, Some y, None
    | Some x, None, Some y
    | None, Some x, Some y -> `Ambiguous (x, y)
    | Some _, Some _, Some _ -> failwith "Unexpected"

let transitions (t : t) : ((int64 * int64) * entry) list =
  let rec aux acc l =
    match l with
    | [] -> List.rev acc
    | [ (k, entry) ] ->
      aux
        (((k, Ptime.(max |> to_float_s |> Int64.of_float)), entry) :: acc)
        []
    | (k1, entry1) :: (k2, entry2) :: rest ->
      aux (((k1, k2), entry1) :: acc) ((k2, entry2) :: rest)
  in
  Array.to_list t.table |> aux []
