include Timedesc_tzdb

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

let recorded_offsets (t : t) : int list =
  Array.to_list t.record.recorded_offsets
  |> List.sort_uniq compare

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
  if size = 0 then invalid_arg "Time zone record table is empty"
  else
    let starts, entries =
      let first_start = starts.{0} in
      let first_entry = entries.(0) in
      if Span.get_s Constants.timestamp_min < first_start then (
        let starts' =
          Bigarray.Array1.create Bigarray.Int64 Bigarray.c_layout (size + 1)
        in
        let sub = Bigarray.Array1.sub starts' 1 size in
        starts'.{0} <- Span.get_s Constants.timestamp_min;
        Bigarray.Array1.blit starts sub;
        (starts', Array.append [| first_entry |] entries))
      else (
        starts.{0} <- Span.get_s Constants.timestamp_min;
        (starts, entries))
    in
    let recorded_offsets =
      Array.fold_left
        (fun acc entry -> Int_set.add entry.offset acc)
        Int_set.empty entries
      |> Int_set.to_seq
      |> Array.of_seq
    in
    let table = (starts, entries) in
    assert (check_table table);
    { recorded_offsets; table }

let name t =
  match t.typ with
  | Backed name -> name
  | Offset_only s ->
    let dur = Span.(make_small ~s () |> For_human'.view) in
    if dur.hours = 0 && dur.minutes = 0 then "UTC"
    else
      Printf.sprintf "UTC%c%02d:%02d"
        (match dur.sign with `Pos -> '+' | `Neg -> '-')
        dur.hours dur.minutes

let to_fixed_offset_from_utc t =
  match t.typ with
  | Backed _ -> None
  | Offset_only s -> Some (Span.make_small ~s ())

let fixed_offset_name_parser =
  let open Angstrom in
  let open Parser_components in
  string "UTC"
  *> ((char '+'
         *> return `Pos
            <|> (char '-' *> return `Neg)
         >>= fun sign ->
         (max_two_digit_nat_zero
            >>= fun hour ->
            char ':'
            *> max_two_digit_nat_zero
            <* end_of_input
            >>= fun minute -> return (hour, minute))
         <|> (max_two_digit_nat_zero <* end_of_input >>= fun hour -> return (hour, 0))
         >>= fun (hour, minute) ->
         if hour < 24 && minute < 60 then
           return (Span.For_human'.make_exn ~sign ~hours:hour ~minutes:minute ())
         else fail "Invalid offset")
      <|> (end_of_input *> return Span.zero))

let fixed_offset_of_name (s : string) : Span.t option =
  match
    Angstrom.(parse_string ~consume:All fixed_offset_name_parser) s
  with
  | Ok x -> Some x
  | Error _ -> None

let equal t1 t2 =
  (match (t1.typ, t2.typ) with
   | Backed name1, Backed name2 -> String.equal name1 name2
   | Offset_only s1, Offset_only s2 -> Int.equal s1 s2
   | Backed name, Offset_only offset | Offset_only offset, Backed name -> (
       match fixed_offset_of_name name with
       | Some offset' -> offset = Int64.to_int @@ Span.get_s offset'
       | None -> false))
  && Bigarray.Array1.dim (fst t1.record.table)
     = Bigarray.Array1.dim (fst t2.record.table)
  && Array.length (snd t1.record.table) = Array.length (snd t2.record.table)
  && Array_utils.for_all2
    (fun e1 e2 -> e1 = e2)
    (snd t1.record.table) (snd t2.record.table)

let one_day = Span.For_human'.(make_exn ~days:1 ())

let make_offset_only (offset : Span.t) =
  if Span.abs offset > one_day then None
  else
    let offset = Int64.to_int @@ Span.get_s offset in
    Some
      {
        typ = Offset_only offset;
        record =
          process_table
            ( Bigarray.Array1.of_array Bigarray.Int64 Bigarray.C_layout
                [| Span.get_s Constants.timestamp_min |],
              [| { is_dst = false; offset } |] );
      }

let make_offset_only_exn offset =
  Misc_utils.option_get_exn_or
    "make_offset_only_span_exn"
    (make_offset_only offset)

let utc : t =
  Misc_utils.option_get_exn_or "Expected successful construction of UTC"
    (make_offset_only Span.zero)

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
    if i = size - 1 then Span.get_s Constants.timestamp_max else starts.{i + 1}
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
                ( ((k1, Span.get_s Constants.timestamp_max), entry1),
                  aux Seq.empty )
          | Seq.Cons ((k2, entry2), rest) ->
            fun () ->
              Seq.Cons
                ( ((k1, k2), entry1),
                  aux (fun () -> Seq.Cons ((k2, entry2), rest)) ))
    in
    Seq_utils_.zero_to_n_exc size
    |> Seq.map (fun i -> (starts.{i}, entries.(i))) |> aux

  let to_transitions (t : t) : ((int64 * int64) * entry) list =
    List.of_seq @@ to_transition_seq t

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

  let of_table ~name table =
    match fixed_offset_of_name name with
    | Some offset -> make_offset_only offset
    | None ->
      Some { typ = Backed name; record = process_table table }

  let of_table_exn ~name table =
    Misc_utils.option_get_exn_or "Failed to construct time zone from table"
      (of_table ~name table)

  let of_transitions ~name (l : (int64 * entry) list) : t option =
    match table_of_transitions l with
    | None -> None
    | Some table ->
      of_table ~name table
end

let offset_is_recorded offset (t : t) =
  Array.mem (Int64.to_int @@ Span.get_s offset) t.record.recorded_offsets

module Compressed_table = struct
  type relative_entry = {
    value : int64;
    is_abs : bool;
    is_dst : bool;
    offset : int;
  }

  let lt_relative_entry (x : relative_entry) (y : relative_entry) =
    if x.value < y.value then
      true
    else
    if x.is_abs && not y.is_abs then
      true
    else
    if x.is_dst && not y.is_dst then
      true
    else
      x.offset < y.offset

  let equal_relative_entry (x : relative_entry) (y : relative_entry) =
    x.value = y.value
    && x.is_abs = y.is_abs
    && x.is_dst = y.is_dst
    && x.offset = y.offset

  module Relative_entry_set = Set.Make (struct
      type t = relative_entry

      let compare x y =
        if lt_relative_entry x y then -1
        else if equal_relative_entry x y then 0
        else 1
    end)

  let to_relative_entries ((starts, entries) : table)
    : relative_entry array * int array =
    let count = Array.length entries in
    let relative_entries =
      Array.init count (fun i ->
          if i = 0 then
            { value = starts.{i};
              is_abs = true;
              is_dst = entries.(i).is_dst;
              offset = entries.(i).offset;
            }
          else
            let a = starts.{i} in
            let b = starts.{i - 1} in
            let would_overflow =
              b < 0L && a > Int64.(add max_int b)
            in
            let would_underflow =
              b > 0L && a < Int64.(add min_int b)
            in
            if would_overflow || would_underflow then
              { value = a;
                is_abs = true;
                is_dst = entries.(i).is_dst;
                offset = entries.(i).offset;
              }
            else
              { value = Int64.sub a b;
                is_abs = false;
                is_dst = entries.(i).is_dst;
                offset = entries.(i).offset;
              }
        )
    in
    let uniq_relative_entries =
      Array.fold_left (fun acc x ->
          Relative_entry_set.add x acc
        ) Relative_entry_set.empty relative_entries
      |> Relative_entry_set.to_seq
      |> Array.of_seq
    in
    let indices =
      Array.init count (fun i ->
          let (index_to_relative_entry, _) =
            Misc_utils.option_get_exn_or "Unexpected failure in relative entry lookup"
            @@
            Misc_utils.array_find_idx
              (fun x -> equal_relative_entry relative_entries.(i) x)
              uniq_relative_entries
          in
          index_to_relative_entry
        )
    in
    (uniq_relative_entries, indices)

  let add_to_buffer (buffer : Buffer.t) (t : table) : unit =
    let (uniq_relative_entries, indices) =
      to_relative_entries t
    in
    let uniq_relative_entry_count =
      Array.length uniq_relative_entries
    in
    assert (uniq_relative_entry_count <= 0xFF);
    Buffer.add_uint8 buffer uniq_relative_entry_count;
    Array.iter (fun (entry : relative_entry) ->
        let offset = Span.make_small ~s:entry.offset () in
        let view = Span.For_human'.view offset in
        let value_is_64bit = Int64.logand entry.value 0xFFFF_FFFF_0000_0000L <> 0L in
        let flags = 0b0000_0000
                    lor (if entry.is_abs      then 0b0100_0000 else 0x00)
                    lor (if value_is_64bit    then 0b0010_0000 else 0x00)
                    lor (if entry.is_dst      then 0b0001_0000 else 0x00)
                    lor (if view.sign = `Pos  then 0b0000_1000 else 0x00)
                    lor (if view.hours > 0    then 0b0000_0100 else 0x00)
                    lor (if view.minutes > 0  then 0b0000_0010 else 0x00)
                    lor (if view.seconds > 0  then 0b0000_0001 else 0x00)
        in
        Buffer.add_uint8 buffer flags;
        if value_is_64bit then
          Buffer.add_int64_be buffer entry.value
        else (
          Buffer.add_int32_be buffer (Int64.to_int32 entry.value)
        );
        if view.hours > 0 then (
          Buffer.add_int8 buffer view.hours
        );
        if view.minutes > 0 then (
          Buffer.add_int8 buffer view.minutes
        );
        if view.seconds > 0 then (
          Buffer.add_int8 buffer view.seconds
        );
      ) uniq_relative_entries;
    let index_count = Array.length indices in
    assert (index_count <= 0xFFFF);
    Buffer.add_uint16_be buffer index_count;
    Array.iter (fun i ->
        Buffer.add_int8 buffer i
      ) indices

  let to_string (t : table) : string =
    let buffer = Buffer.create 512 in
    add_to_buffer buffer t;
    Buffer.contents buffer

  module Parsers = struct
    open Angstrom

    let offset_p ~is_pos ~hour_nz ~minute_nz ~second_nz =
      let sign = if is_pos then `Pos else `Neg in
      (if hour_nz then any_int8 else return 0)
      >>= (fun hours ->
          (if minute_nz then any_int8 else return 0)
          >>= (fun minutes ->
              (if second_nz then any_int8 else return 0)
              >>| (fun seconds ->
                  Span.For_human'.make_exn ~sign ~hours ~minutes ~seconds ()
                  |> Span.get_s
                  |> Int64.to_int
                )
            )
        )

    let relative_entry_p =
      any_uint8 >>= (fun flags ->
          let is_abs         = flags land 0b0100_0000 <> 0 in
          let value_is_64bit = flags land 0b0010_0000 <> 0 in
          let is_dst         = flags land 0b0001_0000 <> 0 in
          let is_pos         = flags land 0b0000_1000 <> 0 in
          let hour_nz        = flags land 0b0000_0100 <> 0 in
          let minute_nz      = flags land 0b0000_0010 <> 0 in
          let second_nz      = flags land 0b0000_0001 <> 0 in
          (if value_is_64bit then
             BE.any_int64
           else
             lift (fun x ->
                 Int64.(logand (of_int32 x) 0xFFFF_FFFFL)
               )
               BE.any_int32)
          >>= (fun value ->
              offset_p ~is_pos ~hour_nz ~minute_nz ~second_nz
              >>| (fun offset -> { value; is_abs; is_dst; offset })
            )
        )

    let relative_table : (relative_entry array * int array) Angstrom.t =
      any_uint8 >>=
      (fun uniq_relative_entry_count ->
         count uniq_relative_entry_count relative_entry_p
         >>= (fun uniq_relative_entries ->
             BE.any_uint16 >>=
             (fun index_count ->
                count index_count any_int8 >>| (fun indices ->
                    (Array.of_list uniq_relative_entries, Array.of_list indices)
                  )
             )
           )
      )

    let table : table option Angstrom.t =
      relative_table >>=
      (fun (uniq_relative_entries, indices) ->
         let size = Array.length indices in
         let starts =
           Bigarray.Array1.create Bigarray.Int64 Bigarray.c_layout size
         in
         let entries =
           Array.make size { is_dst = false; offset = 0 }
         in
         Array.iteri (fun i index ->
             let entry =
               uniq_relative_entries.(index)
             in
             (if entry.is_abs then
                starts.{i} <- entry.value
              else
                starts.{i} <- Int64.add starts.{i - 1} entry.value);
             entries.(i) <- { is_dst = entry.is_dst; offset = entry.offset };
           ) indices;
         let table = (starts, entries) in
         if check_table table then
           return (Some table)
         else
           return None
      )
  end

  let of_string (s : string) : table option =
    let open Angstrom in
    match
      parse_string ~consume:Consume.All Parsers.table s
    with
    | Ok x -> x
    | Error _ -> None

  let of_string_exn s =
    match of_string s with
    | Some x -> x
    | None -> invalid_arg "Failed to deserialize compressed table"
end

module Compressed = struct
  let add_to_buffer
      (buffer : Buffer.t)
      (t : t)
    : unit =
    let name = name t in
    let name_len = String.length name in
    assert (name_len <= 0xFF);
    Buffer.add_uint8 buffer name_len;
    Buffer.add_string buffer name;
    Compressed_table.add_to_buffer buffer t.record.table

  let to_string (t : t) : string =
    let buffer = Buffer.create 512 in
    add_to_buffer buffer t;
    Buffer.contents buffer

  module Parsers = struct
    let p : t option Angstrom.t =
      let open Angstrom in
      any_uint8 >>=
      (fun name_len ->
         take name_len >>=
         (fun name ->
            Compressed_table.Parsers.table >>|
            (fun table ->
               match table with
               | None -> None
               | Some table ->
                 Raw.of_table ~name table
            )
         )
      )
  end

  let of_string (s : string) : t option =
    let open Angstrom in
    match
      parse_string ~consume:Consume.All Parsers.p s
    with
    | Ok x -> x
    | Error _ -> None

  let of_string_exn s =
    match of_string s with
    | Some x -> x
    | None -> invalid_arg "Failed to deserialize compressed time zone"
end

module Db = struct
  type db = table M.t

  let empty = M.empty

  let add tz db = M.add (name tz) tz.record.table db

  let find_opt name db =
    M.find_opt name db |> Option.map (fun table -> Raw.of_table_exn ~name table)

  let remove name db = M.remove name db

  let add_seq db s : db = Seq.fold_left (fun db tz -> add tz db) db s

  let of_seq s : db = add_seq empty s

  let names db = List.map fst (M.bindings db)

  module Compressed = struct
    let to_string (db : db) =
      let buffer = Buffer.create (512 * 1024) in
      let table_count = M.cardinal db in
      Buffer.add_uint16_be buffer table_count;
      M.iter (fun name table ->
          let name_len = String.length name in
          assert (name_len <= 0xFF);
          Buffer.add_uint8 buffer name_len;
          Buffer.add_string buffer name;
          let table_str = Compressed_table.to_string table in
          let table_str_len = String.length table_str in
          assert (table_str_len <= 0xFFFF);
          Buffer.add_uint16_be buffer table_str_len;
          Buffer.add_string buffer table_str;
        ) db;
      Buffer.contents buffer

    module Parsers = struct
      open Angstrom

      let half_compressed_name_and_table : (string * string) Angstrom.t =
        any_uint8 >>=
        (fun name_len ->
           take name_len >>=
           (fun name ->
              BE.any_uint16 >>=
              (fun table_str_len ->
                 take table_str_len >>|
                 (fun table_str ->
                    (name, table_str)
                 )
              )
           )
        )

      let half_compressed : string M.t Angstrom.t =
        BE.any_uint16 >>=
        (fun table_count ->
           count table_count half_compressed_name_and_table >>|
           (fun l ->
              l
              |> List.to_seq
              |> M.of_seq
           )
        )
    end

    let half_compressed_of_string (s : string) : string M.t option =
      let open Angstrom in
      match
        parse_string ~consume:Consume.All Parsers.half_compressed s
      with
      | Ok x -> Some x
      | Error _ -> None

    let half_compressed_of_string_exn s =
      match half_compressed_of_string s with
      | Some x -> x
      | None -> invalid_arg "Failed to deserialize compressed tzdb"

    let of_string s : db option =
      match half_compressed_of_string s with
      | None -> None
      | Some m ->
        try
          Some (M.map Compressed_table.of_string_exn m)
        with
        | _ -> None

    let of_string_exn s : db =
      match of_string s with
      | Some m -> m
      | None -> invalid_arg "Failed to deserialize compressed tzdb"
  end
end

let db : table M.t ref =
  match db with
  | Some db -> ref db
  | None -> ref M.empty

let half_compressed : string M.t =
  match compressed with
  | Some s -> Db.Compressed.half_compressed_of_string_exn s
  | None -> M.empty

let lookup_record name : record option =
  match M.find_opt name !db with
  | Some table ->
    assert (check_table table);
    Some (process_table table)
  | None ->
    match M.find_opt name half_compressed with
    | Some compressed_table ->
      let table =
        Compressed_table.of_string_exn compressed_table
      in
      assert (check_table table);
      db := M.add name table !db;
      Some (process_table table)
    | None -> None

let make name : t option =
  match fixed_offset_of_name name with
  | Some fixed -> make_offset_only fixed
  | None -> (
      match lookup_record name with
      | Some record -> Some { typ = Backed name; record }
      | None -> None)

let make_exn name : t =
  match make name with Some x -> x | None -> invalid_arg "make_exn"

let available_time_zones =
  let s0 =
    M.to_seq !db
    |> Seq.map fst
    |> String_set.of_seq
  in
  let s1 =
    M.to_seq half_compressed
    |> Seq.map fst
    |> String_set.of_seq
  in
  String_set.(union s0 s1 |> to_seq |> List.of_seq)

let local () : t option =
  match Timedesc_tzlocal.local () with
  | [] -> None
  | l ->
    List.fold_left
      (fun tz name -> match tz with Some tz -> Some tz | None -> make name)
      None l

let local_exn () : t =
  Misc_utils.option_get_exn_or
    "local_exn: Could not determine the local timezone"
    (local ())

