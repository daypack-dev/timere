include Timere_tzdb

type record = {
  recorded_offsets : int array;
  table : table;
}

type t = {
  name : string;
  record : record;
}

type 'a local_result =
  [ `None
  | `Single of 'a
  | `Ambiguous of 'a * 'a
  ]

let process_table (table : table) : record =
  let len = Array.length table in
  if len = 0 then failwith "Time zone record table is empty"
  else
    let table =
      let first_row = table.(0) in
      if Constants.min_timestamp < fst first_row then
        Array.append [| (Constants.min_timestamp, snd first_row) |] table
      else (
        table.(0) <- (Constants.min_timestamp, snd first_row);
        table)
    in
    let recorded_offsets =
      Array.fold_left
        (fun acc (_, entry) -> Int_set.add entry.offset acc)
        Int_set.empty table
      |> Int_set.to_list
      |> CCArray.of_list
    in
    { recorded_offsets; table }

let lookup_ref : (string -> table option) ref = ref lookup

let lookup_record name : record option =
  name |> !lookup_ref |> CCOpt.map process_table

let name t = t.name

let equal t1 t2 =
  t1.name = t2.name
  && Array.length t1.record.table = Array.length t2.record.table
  && CCArray.for_all2 (fun e1 e2 -> e1 = e2) t1.record.table t2.record.table

let make name : (t, unit) result =
  match lookup_record name with
  | Some record -> Ok { name; record }
  | None -> Error ()

let make_exn name : t =
  match make name with Ok x -> x | Error () -> invalid_arg "make_exn"

let utc : t =
  {
    name = "UTC";
    record =
      process_table
        [| (Constants.min_timestamp, { is_dst = false; offset = 0 }) |];
  }

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
      Constants.max_timestamp
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
      | Some x, None, None | None, Some x, None | None, None, Some x ->
        `Single x
      | Some x, Some y, None | Some x, None, Some y | None, Some x, Some y ->
        `Ambiguous (x, y)
      | Some _, Some _, Some _ -> failwith "Unexpected case")

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

let make_offset_only ?(name = "dummy") (offset : int) =
  {
    name;
    record =
      process_table [| (Constants.min_timestamp, { is_dst = false; offset }) |];
  }

let of_json_string s : (t, unit) result =
  let exception Invalid_data in
  try
    let json = Yojson.Basic.from_string s in
    match json with
    | `Assoc l ->
      let name =
        match List.assoc "name" l with
        | `String s -> s
        | _ -> raise Invalid_data
      in
      let table_rows =
        match List.assoc "table" l with
        | `List l -> l
        | _ -> raise Invalid_data
      in
      let table =
        table_rows
        |> List.map (fun row ->
            match row with
            | `List [ `String s; `Assoc e ] ->
              let start = Int64.of_string s in
              let is_dst =
                match List.assoc "is_dst" e with
                | `Bool b -> b
                | _ -> raise Invalid_data
              in
              let offset =
                match List.assoc "offset" e with
                | `Int x -> x
                | _ -> raise Invalid_data
              in
              let entry = Timere_tzdb.{ is_dst; offset } in
              (start, entry)
            | _ -> raise Invalid_data)
        |> Array.of_list
      in
      Array.fold_left
        (fun last_start (start, _) ->
           match last_start with
           | None -> Some start
           | Some last_start ->
             if last_start < start then Some start else raise Invalid_data)
        None table
      |> ignore;
      Ok { name; record = process_table table }
    | _ -> raise Invalid_data
  with _ -> Error ()

let to_json_string (t : t) : string =
  let json =
    `Assoc
      [
        ("name", `String t.name);
        ( "table",
          `List
            (Array.to_list t.record.table
             |> List.map (fun (start, entry) ->
                 `List
                   [
                     `String (Int64.to_string start);
                     `Assoc
                       [
                         ("is_dst", `Bool entry.is_dst);
                         ("offset", `Int entry.offset);
                       ];
                   ])) );
      ]
  in
  Yojson.Basic.to_string json
