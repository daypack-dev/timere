include Timere_tzdb

type record = {
  recorded_offsets : int array;
  table : table;
}

type typ =
  | Backed of string
  | Offset_only of int

type t = {
  typ : typ;
  record : record;
}

type 'a local_result =
  [ `None
  | `Single of 'a
  | `Ambiguous of 'a * 'a
  ]

let check_table ((starts, entries) : table) : bool =
  let size = Bigarray.Array1.dim starts in
  assert (size = Array.length entries);
  let has_no_dup =
    let seen = ref Int64_set.empty in
    let has_no_dup = ref true in
    let i = ref 0 in
    while !has_no_dup && !i < size do
      let start = starts.{!i} in
      if Int64_set.mem start !seen then has_no_dup := false
      else seen := Int64_set.add start !seen;
      i := !i + 1
    done;
    !has_no_dup
  in
  let is_sorted =
    let i = ref 0 in
    let is_sorted = ref true in
    while !is_sorted && !i < size do
      (if !i > 0 then
         let cur = starts.{!i} in
         let prev = starts.{!i - 1} in
         if cur < prev then is_sorted := false);
      i := !i + 1
    done;
    !is_sorted
  in
  has_no_dup && is_sorted

let process_table ((starts, entries) : table) : record =
  let size = Bigarray.Array1.dim starts in
  assert (size = Array.length entries);
  if size = 0 then failwith "Time zone record table is empty"
  else
    let starts, entries =
      let first_start = starts.{0} in
      let first_entry = entries.(0) in
      if Constants.timestamp_min.s < first_start then (
        let starts' =
          Bigarray.Array1.create Bigarray.Int64 Bigarray.c_layout (size + 1)
        in
        let sub = Bigarray.Array1.sub starts' 1 size in
        starts'.{0} <- Constants.timestamp_min.s;
        Bigarray.Array1.blit starts sub;
        (starts', Array.append [| first_entry |] entries))
      else (
        starts.{0} <- Constants.timestamp_min.s;
        (starts, entries))
    in
    let recorded_offsets =
      Array.fold_left
        (fun acc entry -> Int_set.add entry.offset acc)
        Int_set.empty entries
      |> Int_set.to_list
      |> CCArray.of_list
    in
    { recorded_offsets; table = (starts, entries) }

let lookup_record name : record option =
  M.find_opt name db
  |> CCOpt.map (fun table ->
      assert (check_table table);
      process_table table)

let name t =
  match t.typ with
  | Backed name -> name
  | Offset_only s ->
    let dur = Duration.of_span Span.(make_small ~s ()) in
    Printf.sprintf "UTC%c%02d:%02d"
      (match dur.sign with `Pos -> '+' | `Neg -> '-')
      dur.hours dur.minutes

let to_fixed_offset t =
  match t.typ with
  | Backed _ -> None
  | Offset_only s -> Some (Duration.of_span (Span.make_small ~s ()))

let equal t1 t2 =
  (match (t1.typ, t2.typ) with
   | Backed name1, Backed name2 -> CCString.equal name1 name2
   | Offset_only s1, Offset_only s2 -> CCInt.equal s1 s2
   | _, _ -> false)
  && Bigarray.Array1.dim (fst t1.record.table)
     = Bigarray.Array1.dim (fst t2.record.table)
  && Array.length (snd t1.record.table) = Array.length (snd t2.record.table)
  && CCArray.for_all2
    (fun e1 e2 -> e1 = e2)
    (snd t1.record.table) (snd t2.record.table)

let make name : t option =
  match lookup_record name with
  | Some record -> Some { typ = Backed name; record }
  | None -> None

let make_exn name : t =
  match make name with Some x -> x | None -> invalid_arg "make_exn"

let utc : t =
  {
    typ = Backed "UTC";
    record =
      process_table
        ( Bigarray.Array1.of_array Bigarray.Int64 Bigarray.C_layout
            [| Constants.timestamp_min.s |],
          [| { is_dst = false; offset = 0 } |] );
  }

let bsearch_table timestamp ((starts, _) : table) =
  Bigarray_utils.bsearch ~cmp:Int64.compare timestamp starts

let lookup_timestamp_utc (t : t) timestamp =
  let table = t.record.table in
  let entries = snd table in
  match bsearch_table timestamp table with
  | `At i -> Some entries.(i)
  | `All_lower -> Some entries.(Array.length entries - 1)
  | `All_bigger -> None
  | `Just_after i -> Some entries.(i)
  | `Empty -> None

let local_interval_of_table ((starts, entries) : table) (i : int) =
  let size = Bigarray.Array1.dim starts in
  let start_utc = starts.{i} in
  let entry = entries.(i) in
  let end_exc_utc =
    if i = size - 1 then Constants.timestamp_max.s else starts.{i + 1}
  in
  ( Int64.add start_utc (Int64.of_int entry.offset),
    Int64.add end_exc_utc (Int64.of_int entry.offset) )

let interval_mem (t : int64) ((x, y) : int64 * int64) = x <= t && t < y

let lookup_timestamp_local (t : t) timestamp : entry local_result =
  let table = t.record.table in
  let starts, entries = table in
  let size = Bigarray.Array1.dim starts in
  let index =
    match bsearch_table timestamp table with
    | `At i -> Some i
    | `All_lower -> Some (size - 1)
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
        then Some entries.(index - 1)
        else None
      in
      let x2 =
        if interval_mem timestamp (local_interval_of_table table index) then
          Some entries.(index)
        else None
      in
      let x3 =
        if
          index < size - 1
          && interval_mem timestamp (local_interval_of_table table (index + 1))
        then Some entries.(index + 1)
        else None
      in
      match (x1, x2, x3) with
      | None, None, None -> `None
      | Some x, None, None | None, Some x, None | None, None, Some x ->
        `Single x
      | Some x, Some y, None | Some x, None, Some y | None, Some x, Some y ->
        `Ambiguous (x, y)
      | Some _, Some _, Some _ -> failwith "Unexpected case")

module Raw = struct
  let to_transition_seq (t : t) : ((int64 * int64) * entry) Seq.t =
    let table = t.record.table in
    let starts, entries = table in
    let size = Bigarray.Array1.dim starts in
    let rec aux s =
      match s () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((k1, entry1), s) -> (
          match s () with
          | Seq.Nil ->
            fun () ->
              Seq.Cons
                (((k1, Constants.timestamp_max.s), entry1), aux Seq.empty)
          | Seq.Cons ((k2, entry2), rest) ->
            fun () ->
              Seq.Cons
                ( ((k1, k2), entry1),
                  aux (fun () -> Seq.Cons ((k2, entry2), rest)) ))
    in
    OSeq.(0 --^ size) |> OSeq.map (fun i -> (starts.{i}, entries.(i))) |> aux

  let to_transitions (t : t) : ((int64 * int64) * entry) list =
    CCList.of_seq @@ to_transition_seq t

  let table_of_transitions (l : (int64 * entry) list) : table option =
    let table =
      l
      |> List.split
      |> fun (starts, entries) ->
      let starts =
        starts
        |> Array.of_list
        |> Bigarray.Array1.of_array Bigarray.Int64 Bigarray.C_layout
      in
      let entries = Array.of_list entries in
      (starts, entries)
    in
    if check_table table then Some table else None

  let of_table ~name table = { typ = Backed name; record = process_table table }

  let of_transitions ~name (l : (int64 * entry) list) : t option =
    table_of_transitions l
    |> CCOpt.map (fun table ->
        { typ = Backed name; record = process_table table })
end

let offset_is_recorded offset (t : t) =
  Array.mem
    (CCInt64.to_int (Duration.to_span offset).s)
    t.record.recorded_offsets

let make_offset_only_span (offset : Span.t) =
  let offset = CCInt64.to_int offset.s in
  {
    typ = Offset_only offset;
    record =
      process_table
        ( Bigarray.Array1.of_array Bigarray.Int64 Bigarray.C_layout
            [| Constants.timestamp_min.s |],
          [| { is_dst = false; offset } |] );
  }

let make_offset_only (offset : Duration.t) =
  make_offset_only_span (Duration.to_span offset)

module Sexp = struct
  let of_sexp (x : CCSexp.t) : t option =
    let open Of_sexp_utils in
    try
      match x with
      | `List l -> (
          match l with
          | `Atom "tz" :: `Atom name :: transitions ->
            transitions
            |> List.map (fun x ->
                match x with
                | `List [ start; `List [ `Atom is_dst; offset ] ] ->
                  let start = int64_of_sexp start in
                  let is_dst =
                    match is_dst with
                    | "t" -> true
                    | "f" -> false
                    | _ -> invalid_data ""
                  in
                  let offset = int_of_sexp offset in
                  let entry = { is_dst; offset } in
                  (start, entry)
                | _ -> invalid_data "")
            |> Raw.of_transitions ~name
          | _ -> invalid_data "")
      | `Atom _ -> invalid_data ""
    with _ -> None

  let to_sexp (t : t) : CCSexp.t =
    let open To_sexp_utils in
    CCSexp.(
      list
        (atom "tz"
         :: atom (name t)
         :: List.map
           (fun ((start, _), entry) ->
              list
                [
                  sexp_of_int64 start;
                  list
                    [
                      (if entry.is_dst then atom "t" else atom "f");
                      sexp_of_int entry.offset;
                    ];
                ])
           (Raw.to_transitions t)))

  let of_string s =
    let res =
      try CCSexp.parse_string s
      with _ -> Error "Failed to parse string into sexp"
    in
    match res with Error _ -> None | Ok x -> of_sexp x
end

module JSON = struct
  let of_json json : t option =
    let exception Invalid_data in
    try
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
              let entry = { is_dst; offset } in
              (start, entry)
            | _ -> raise Invalid_data)
        |> Raw.of_transitions ~name
      | _ -> raise Invalid_data
    with _ -> None

  let of_string s = try of_json @@ Yojson.Basic.from_string s with _ -> None

  let to_json (t : t) : Yojson.Basic.t =
    `Assoc
      [
        ("name", `String (name t));
        ( "table",
          `List
            (Raw.to_transition_seq t
             |> Seq.map (fun ((start, _), entry) ->
                 `List
                   [
                     `String (Int64.to_string start);
                     `Assoc
                       [
                         ("is_dst", `Bool entry.is_dst);
                         ("offset", `Int entry.offset);
                       ];
                   ])
             |> CCList.of_seq) );
      ]
end

module Db = struct
  type db = table M.t

  let empty = M.empty

  let add tz db = M.add (name tz) tz.record.table db

  let find_opt name db =
    M.find_opt name db |> CCOpt.map (fun table -> Raw.of_table ~name table)

  let remove name db = M.remove name db

  let add_seq db s : db = Seq.fold_left (fun db tz -> add tz db) db s

  let of_seq s : db = add_seq empty s

  let names db = List.map fst (M.bindings db)

  module Raw' = Raw

  module Raw = struct
    let dump (db : db) : string = Marshal.to_string db []

    let load s : db = Marshal.from_string s 0
  end

  module Sexp = struct
    let of_sexp (x : CCSexp.t) : db option =
      let open Of_sexp_utils in
      try
        match x with
        | `Atom _ -> invalid_data ""
        | `List l ->
          Some
            (l
             |> CCList.to_seq
             |> Seq.map (fun x ->
                 match Sexp.of_sexp x with
                 | None -> invalid_data ""
                 | Some x -> x)
             |> of_seq)
      with _ -> None

    let to_sexp db =
      M.bindings db
      |> List.map (fun (name, table) -> Raw'.of_table ~name table)
      |> List.map Sexp.to_sexp
      |> CCSexp.list

    let of_string s =
      let res =
        try CCSexp.parse_string s
        with _ -> Error "Failed to parse string into sexp"
      in
      match res with Error _ -> None | Ok x -> of_sexp x
  end
end

let available_time_zones = Db.names db

let local () : t option =
  match Timere_tzlocal.local () with
  | [] -> None
  | l ->
    List.fold_left
      (fun tz name -> match tz with Some tz -> Some tz | None -> make name)
      None l

let local_exn () : t =
  match local () with
  | None -> invalid_arg "local_exn: Could not determine the local timezone"
  | Some x -> x
