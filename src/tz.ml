include Tz_data

type t = {
  name : string;
  table : table;
}

let is_utc t = t.name = "UTC"

let available_time_zones = String_map.bindings db |> List.map (fun (k, _) -> k)

let make name : t =
  match String_map.find_opt name db with
  | Some table -> { name; table }
  | None -> failwith "make: Invalid time zone name"

let dummy_entry : entry = { is_dst = false; offset = 0 }

let lookup_timestamp (t : t) timestamp =
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
