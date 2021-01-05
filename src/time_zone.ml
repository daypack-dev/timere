include Time_zone_data

type t = {
  name : string;
  record : record;
}

type 'a local_result =
  [ `None
  | `Exact of 'a
  | `Ambiguous of 'a * 'a
  ]

let name t = t.name

let equal t1 t2 = t1.name = t2.name

let available_time_zones = String_map.bindings db |> List.map (fun (k, _) -> k)

let make name : (t, unit) result =
  match String_map.find_opt name db with
  | Some record -> Ok { name; record }
  | None -> Error ()

let make_exn name : t =
  match make name with Ok x -> x | Error () -> invalid_arg "make_exn"

let utc = make_exn "UTC"

let dummy_entry : entry = { is_dst = false; offset = 0 }

let bsearch_table timestamp (table : table) =
  CCArray.bsearch
    ~cmp:(fun (k1, _) (k2, _) -> Int64.compare k1 k2)
    (timestamp, dummy_entry) table

let lookup_timestamp_utc (t : t) timestamp =
  let table = t.record.table in
  match bsearch_table timestamp table with
  | `At i -> Some (snd table.(i))
  | `All_lower -> Some (snd table.(Array.length table - 1))
  | `All_bigger -> None
  | `Just_after i -> Some (snd table.(i))
  | `Empty -> None

let local_interval_of_table (table : table) (i : int) =
  let start_utc, entry = table.(i) in
  let end_exc_utc =
    if i = Array.length table - 1 then
      Ptime.(max |> to_float_s |> Int64.of_float)
    else fst table.(i + 1)
  in
  ( Int64.add start_utc (Int64.of_int entry.offset),
    Int64.add end_exc_utc (Int64.of_int entry.offset) )

let interval_mem (t : int64) ((x, y) : int64 * int64) = x <= t && t < y

let lookup_timestamp_local (t : t) timestamp : entry local_result =
  let table = t.record.table in
  let index =
    match bsearch_table timestamp table with
    | `At i -> Some i
    | `All_lower -> Some (Array.length table - 1)
    | `All_bigger -> Some 0
    | `Just_after i -> Some i
    | `Empty -> None
  in
  match index with
  | None -> `None
  | Some index -> (
      let x1 =
        if
          index > 0
          && interval_mem timestamp (local_interval_of_table table (index - 1))
        then Some (snd table.(index - 1))
        else None
      in
      let x2 =
        if interval_mem timestamp (local_interval_of_table table index) then
          Some (snd table.(index))
        else None
      in
      let x3 =
        if
          index < Array.length table - 1
          && interval_mem timestamp (local_interval_of_table table (index + 1))
        then Some (snd table.(index + 1))
        else None
      in
      match (x1, x2, x3) with
      | None, None, None -> `None
      | Some x, None, None | None, Some x, None | None, None, Some x -> `Exact x
      | Some x, Some y, None | Some x, None, Some y | None, Some x, Some y ->
        `Ambiguous (x, y)
      | Some _, Some _, Some _ -> failwith "Unexpected")

let transition_seq (t : t) : ((int64 * int64) * entry) Seq.t =
  let table = t.record.table in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((k1, entry1), s) -> (
        match s () with
        | Seq.Nil ->
          fun () ->
            Seq.Cons (((k1, Constants.max_timestamp), entry1), aux Seq.empty)
        | Seq.Cons ((k2, entry2), rest) ->
          fun () ->
            Seq.Cons
              ( ((k1, k2), entry1),
                aux (fun () -> Seq.Cons ((k2, entry2), rest)) ))
  in
  CCArray.to_seq table |> aux

let transitions (t : t) : ((int64 * int64) * entry) list =
  CCList.of_seq @@ transition_seq t

let offset_is_recorded offset (t : t) =
  Array.mem offset t.record.recorded_offsets
