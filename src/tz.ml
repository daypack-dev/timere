include Tz_data

type t = {
  name : string;
  table : table;
}

let available_time_zones =
  String_map.bindings db
  |> List.map (fun (k, _) -> k)

let make name : t =
  match String_map.find_opt name db with
  | Some table ->
    { name; table }
  | None ->
    failwith "make: Invalid time zone name"

let lookup_timestamp (t : t) timestamp =
  let (l, exact, _r) = Int64_map.split timestamp t.table in
  match exact with
  | Some x -> Some x
  | None ->
    match Int64_map.max_binding_opt l with
    | Some (_k, v) -> Some v
    | None -> None
