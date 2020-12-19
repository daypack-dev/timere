include Tz_data

type t = {
  name : string;
  table : table;
}

let is_utc t = t.name = "UTC"

let available_time_zones =
  String_map.bindings db_utc
  |> List.map (fun (k, _) -> k)

let make name : t =
  match String_map.find_opt name db_utc with
  | Some table ->
    { name; table }
  | None ->
    failwith "make: Invalid time zone name"

let lookup_timestamp (t : t) timestamp =
  if is_utc t then
    Some { is_dst = false; offset = 0; }
  else
    let (l, exact, _r) = Int64_map.split timestamp t.table in
    match exact with
    | Some x -> Some x
    | None ->
      match Int64_map.max_binding_opt l with
      | Some (_k, v) -> Some v
      | None -> None

let transitions (t : t) : ((int64 * int64) * entry) list =
  let rec aux acc l =
    match l with
    | [] -> List.rev acc
    | [(k, entry)] -> aux (((k, Ptime.(max |> to_float_s |> Int64.of_float)), entry) :: acc) []
    | (k1, entry1) :: (k2, entry2) :: rest ->
      aux (((k1, k2), entry1) :: acc) ((k2, entry2) :: rest)
  in
  Int64_map.bindings t.table
  |> aux []
