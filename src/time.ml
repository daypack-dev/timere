open Date_time_components
open Time_ast

type tz_offset_s = int

let tz_offset_s_utc = 0

exception Invalid_timestamp

exception Interval_is_invalid

exception Interval_is_empty

exception Intervals_are_not_sorted

exception Intervals_are_not_disjoint

type timestamp = Span.t

let one_ns = Span.make ~ns:1 ()

module Interval' = struct
  type t = timestamp * timestamp

  let lt (x1, y1) (x2, y2) =
    (* lexicographic order *)
    Span.(x1 < x2 || (x1 = x2 && y1 < y2))

  let le x y = lt x y || x = y

  let gt x y = lt y x

  let ge x y = le y x

  let equal (x1, y1) (x2, y2) = Span.(x1 = x2 && y1 = y2)

  let compare x y = if lt x y then -1 else if x = y then 0 else 1

  module Check = struct
    let is_valid ((start, end_exc) : t) : bool = Span.(start <= end_exc)

    let is_not_empty ((start, end_exc) : t) : bool = not Span.(start = end_exc)

    let check_if_valid (x : t) : t =
      if is_valid x then x else raise Interval_is_invalid

    let check_if_not_empty (x : t) : t =
      if is_not_empty x then x else raise Interval_is_empty
  end

  let join (ts1 : t) (ts2 : t) : t option =
    let open Span in
    let aux (start1, end_exc1) (start2, end_exc2) =
      if start2 <= end_exc1 then Some (start1, max end_exc1 end_exc2) else None
    in
    let start1, end_exc1 = Check.check_if_valid ts1 in
    let start2, end_exc2 = Check.check_if_valid ts2 in
    if start1 <= start2 then aux (start1, end_exc1) (start2, end_exc2)
    else aux (start2, end_exc2) (start1, end_exc1)

  let overlap_of_a_over_b ~(a : t) ~(b : t) : t option * t option * t option =
    let open Span in
    let a_start, a_end_exc = Check.check_if_valid a in
    let b_start, b_end_exc = Check.check_if_valid b in
    if a_start = a_end_exc then (None, None, None)
    else if a_end_exc <= b_start then (Some a, None, None)
    else if b_end_exc <= a_start then (None, None, Some a)
    else if a_start < b_start then
      if a_end_exc <= b_end_exc then
        (Some (a_start, b_start), Some (b_start, a_end_exc), None)
      else (Some (a_start, b_start), Some b, Some (b_end_exc, a_end_exc))
    else if a_end_exc <= b_end_exc then (None, Some (a_start, a_end_exc), None)
    else (None, Some (a_start, a_end_exc), Some (b_end_exc, a_end_exc))
end

module Intervals = struct
  module Check = struct
    let check_if_valid (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      Seq.map Interval'.Check.check_if_valid intervals

    let check_if_valid_list (intervals : Interval'.t list) : Interval'.t list =
      List.map Interval'.Check.check_if_valid intervals

    let check_if_not_empty (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      Seq.map Interval'.Check.check_if_not_empty intervals

    let check_if_sorted (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      Seq_utils.check_if_f_holds_for_immediate_neighbors ~f:Interval'.le
        ~f_exn:(fun _ _ -> Intervals_are_not_sorted)
        intervals

    let check_if_sorted_rev (intervals : Interval'.t Seq.t) : Interval'.t Seq.t
      =
      Seq_utils.check_if_f_holds_for_immediate_neighbors ~f:Interval'.ge
        ~f_exn:(fun _ _ -> Intervals_are_not_sorted)
        intervals

    let check_if_disjoint (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      Seq_utils.check_if_f_holds_for_immediate_neighbors
        ~f:(fun x y ->
            match Interval'.overlap_of_a_over_b ~a:y ~b:x with
            | None, None, None | Some _, None, None | None, None, Some _ -> true
            | _ -> false)
        ~f_exn:(fun _ _ -> Intervals_are_not_disjoint)
        intervals

    let check_if_normalized (intervals : Interval'.t Seq.t) : Interval'.t Seq.t
      =
      intervals
      |> check_if_valid
      |> check_if_not_empty
      |> check_if_sorted
      |> check_if_disjoint
  end

  module Filter = struct
    let filter_invalid (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      Seq.filter Interval'.Check.is_valid intervals

    let filter_invalid_list (intervals : Interval'.t list) : Interval'.t list =
      List.filter Interval'.Check.is_valid intervals

    let filter_empty (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      Seq.filter Interval'.Check.is_not_empty intervals

    let filter_empty_list (intervals : Interval'.t list) : Interval'.t list =
      List.filter Interval'.Check.is_not_empty intervals
  end

  module Sort = struct
    let sort_intervals_list ?(skip_check = false) (intervals : Interval'.t list)
      : Interval'.t list =
      intervals
      |> (fun l ->
          if skip_check then l
          else l |> CCList.to_seq |> Check.check_if_valid |> CCList.of_seq)
      |> List.sort Interval'.compare

    let sort_uniq_intervals_list ?(skip_check = false)
        (intervals : Interval'.t list) : Interval'.t list =
      intervals
      |> (fun l ->
          if skip_check then l
          else l |> CCList.to_seq |> Check.check_if_valid |> CCList.of_seq)
      |> List.sort_uniq Interval'.compare

    let sort_uniq_intervals ?(skip_check = false)
        (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      intervals
      |> (fun s -> if skip_check then s else Check.check_if_valid s)
      |> CCList.of_seq
      |> List.sort_uniq Interval'.compare
      |> CCList.to_seq

    let sort_intervals ?(skip_check = false) (intervals : Interval'.t Seq.t) :
      Interval'.t Seq.t =
      intervals
      |> (fun s -> if skip_check then s else Check.check_if_valid s)
      |> CCList.of_seq
      |> List.sort Interval'.compare
      |> CCList.to_seq
  end

  module Join_internal = struct
    let join (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      let rec aux cur intervals =
        match intervals () with
        | Seq.Nil -> Seq.return cur
        | Seq.Cons ((start, end_exc), rest) -> (
            match Interval'.join cur (start, end_exc) with
            | Some x -> aux x rest
            | None ->
              (* cannot be merged, add time slot being carried to the sequence *)
              fun () -> Seq.Cons (cur, aux (start, end_exc) rest))
      in
      let aux' intervals =
        match intervals () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons ((start, end_exc), rest) -> aux (start, end_exc) rest
      in
      aux' intervals
  end

  let join ?(skip_check = false) intervals =
    intervals
    |> (fun s ->
        if skip_check then s
        else s |> Check.check_if_valid |> Check.check_if_sorted)
    |> Join_internal.join

  let normalize ?(skip_filter_invalid = false) ?(skip_filter_empty = false)
      ?(skip_sort = false) intervals =
    intervals
    |> (fun s -> if skip_filter_invalid then s else Filter.filter_invalid s)
    |> (fun s -> if skip_filter_empty then s else Filter.filter_empty s)
    |> (fun s -> if skip_sort then s else Sort.sort_uniq_intervals s)
    |> Join_internal.join

  module Slice_internal = struct
    let slice_start ~start (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
      let open Span in
      let rec aux start intervals =
        match intervals () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons ((ts_start, ts_end_exc), rest) ->
          if start <= ts_start then
            (* entire time slot is follow start, do nothing *)
            intervals
          else if ts_start < start && start < ts_end_exc then
            (* time slot spans across the start mark, split time slot *)
            fun () -> Seq.Cons ((start, ts_end_exc), rest)
          else
            (* time slot is before start mark, move to next time slot *)
            aux start rest
      in
      aux start intervals

    let slice_end_exc ~end_exc (intervals : Interval'.t Seq.t) :
      Interval'.t Seq.t =
      let open Span in
      let rec aux end_exc intervals =
        match intervals () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons ((ts_start, ts_end_exc), rest) ->
          if end_exc <= ts_start then
            (* entire time slot is follow end_exc mark, drop everything *)
            aux end_exc Seq.empty
          else if ts_start < end_exc && end_exc < ts_end_exc then
            (* time slot spans across the end_exc mark, split time slot,
               skip remaining slots *)
            fun () -> Seq.Cons ((ts_start, end_exc), aux end_exc Seq.empty)
          else
            (* time slot is before end_exc, add to sequence and move to next time slot *)
            fun () -> Seq.Cons ((ts_start, ts_end_exc), aux end_exc rest)
      in
      aux end_exc intervals
  end

  module Slice = struct
    let slice ?(skip_check = false) ?start ?end_exc intervals =
      intervals
      |> (fun s ->
          if skip_check then s
          else s |> Check.check_if_valid |> Check.check_if_sorted)
      |> (fun s ->
          match start with
          | None -> s
          | Some start -> Slice_internal.slice_start ~start s)
      |> fun s ->
      match end_exc with
      | None -> s
      | Some end_exc -> Slice_internal.slice_end_exc ~end_exc s
  end

  let relative_complement ?(skip_check = false)
      ~(not_mem_of : Interval'.t Seq.t) (mem_of : Interval'.t Seq.t) :
    Interval'.t Seq.t =
    let rec aux mem_of not_mem_of =
      match (mem_of (), not_mem_of ()) with
      | Seq.Nil, _ -> Seq.empty
      | _, Seq.Nil -> mem_of
      | ( Seq.Cons (mem_of_ts, mem_of_rest),
          Seq.Cons (not_mem_of_ts, not_mem_of_rest) ) -> (
          let mem_of () = Seq.Cons (mem_of_ts, mem_of_rest) in
          let not_mem_of () = Seq.Cons (not_mem_of_ts, not_mem_of_rest) in
          match Interval'.overlap_of_a_over_b ~a:mem_of_ts ~b:not_mem_of_ts with
          | None, None, None ->
            (* mem_of_ts is empty, drop mem_of_ts *)
            aux mem_of_rest not_mem_of
          | Some _, None, None ->
            (* mem_of_ts is before not_mem_of_ts entirely, output mem_of *)
            fun () -> Seq.Cons (mem_of_ts, aux mem_of_rest not_mem_of)
          | None, None, Some _ ->
            (* not_mem_of_ts is before mem_of entirely, drop not_mem_of_ts *)
            aux mem_of not_mem_of_rest
          | Some (start, end_exc), Some _, None ->
            fun () -> Seq.Cons ((start, end_exc), aux mem_of_rest not_mem_of)
          | None, Some _, None -> aux mem_of_rest not_mem_of
          | None, Some _, Some (start, end_exc) ->
            let mem_of () = Seq.Cons ((start, end_exc), mem_of_rest) in
            aux mem_of not_mem_of_rest
          | Some (start1, end_exc1), _, Some (start2, end_exc2) ->
            let mem_of () = Seq.Cons ((start2, end_exc2), mem_of_rest) in
            fun () -> Seq.Cons ((start1, end_exc1), aux mem_of not_mem_of_rest)
        )
    in
    let mem_of =
      if skip_check then mem_of
      else mem_of |> Check.check_if_valid |> Check.check_if_sorted
    in
    let not_mem_of =
      if skip_check then not_mem_of
      else not_mem_of |> Check.check_if_valid |> Check.check_if_sorted
    in
    aux mem_of not_mem_of

  let invert ?(skip_check = false) ~start ~end_exc
      (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
    relative_complement ~skip_check ~not_mem_of:intervals
      (Seq.return (start, end_exc))

  module Inter = struct
    let inter ?(skip_check = false) (intervals1 : Interval'.t Seq.t)
        (intervals2 : Interval'.t Seq.t) : Interval'.t Seq.t =
      let open Span in
      let rec aux intervals1 intervals2 : Interval'.t Seq.t =
        match (intervals1 (), intervals2 ()) with
        | Seq.Nil, _ -> Seq.empty
        | _, Seq.Nil -> Seq.empty
        | ( Seq.Cons ((start1, end_exc1), rest1),
            Seq.Cons ((start2, end_exc2), rest2) ) ->
          if end_exc1 < start2 then
            (* 1 is before 2 entirely, drop 1, keep 2 *)
            aux rest1 intervals2
          else if end_exc2 < start1 then
            (* 2 is before 1 entirely, keep 1, drop 2 *)
            aux intervals1 rest2
          else
            (* there is an overlap or touching *)
            let overlap_start = max start1 start2 in
            let overlap_end_exc = min end_exc1 end_exc2 in
            let s1 =
              if end_exc1 <= overlap_end_exc then rest1 else intervals1
            in
            let s2 =
              if end_exc2 <= overlap_end_exc then rest2 else intervals2
            in
            if overlap_start < overlap_end_exc then
              (* there is an overlap *)
              fun () -> Seq.Cons ((overlap_start, overlap_end_exc), aux s1 s2)
            else aux s1 s2
      in
      let intervals1 =
        if skip_check then intervals1
        else intervals1 |> Check.check_if_valid |> Check.check_if_sorted
      in
      let intervals2 =
        if skip_check then intervals2
        else intervals2 |> Check.check_if_valid |> Check.check_if_sorted
      in
      aux intervals1 intervals2

    let inter_multi_seq ?(skip_check = false)
        (interval_batches : Interval'.t Seq.t Seq.t) : Interval'.t Seq.t =
      match
        Seq.fold_left
          (fun acc intervals ->
             match acc with
             | None -> Some intervals
             | Some acc -> Some (inter ~skip_check acc intervals))
          None interval_batches
      with
      | None -> Seq.empty
      | Some s -> s
  end

  module Merge = struct
    let merge ?(skip_check = false) (intervals1 : Interval'.t Seq.t)
        (intervals2 : Interval'.t Seq.t) : Interval'.t Seq.t =
      let rec aux intervals1 intervals2 =
        match (intervals1 (), intervals2 ()) with
        | Seq.Nil, s | s, Seq.Nil -> fun () -> s
        | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
          if Interval'.le x1 x2 then fun () ->
            Seq.Cons (x1, aux rest1 intervals2)
          else fun () -> Seq.Cons (x2, aux rest2 intervals1)
      in
      let intervals1 =
        if skip_check then intervals1
        else intervals1 |> Check.check_if_valid |> Check.check_if_sorted
      in
      let intervals2 =
        if skip_check then intervals2
        else intervals2 |> Check.check_if_valid |> Check.check_if_sorted
      in
      aux intervals1 intervals2

    let merge_multi_seq ?(skip_check = false)
        (interval_batches : Interval'.t Seq.t Seq.t) : Interval'.t Seq.t =
      Seq.fold_left
        (fun acc intervals -> merge ~skip_check acc intervals)
        Seq.empty interval_batches
  end

  module Union = struct
    let union ?(skip_check = false) (intervals1 : Interval'.t Seq.t)
        (intervals2 : Interval'.t Seq.t) : Interval'.t Seq.t =
      Merge.merge ~skip_check intervals1 intervals2
      |> normalize ~skip_filter_invalid:true ~skip_sort:true

    let union_multi_seq ?(skip_check = false)
        (interval_batches : Interval'.t Seq.t Seq.t) : Interval'.t Seq.t =
      Seq.fold_left
        (fun acc intervals -> union ~skip_check acc intervals)
        Seq.empty interval_batches
  end

  module Round_robin = struct
    let collect_round_robin_non_decreasing ?(skip_check = false)
        (batches : Interval'.t Seq.t list) : Interval'.t option list Seq.t =
      batches
      |> List.map (fun s ->
          if skip_check then s
          else s |> Check.check_if_valid |> Check.check_if_sorted)
      |> Seq_utils.collect_round_robin ~f_le:Interval'.le

    let merge_multi_list_round_robin_non_decreasing ?(skip_check = false)
        (batches : Interval'.t Seq.t list) : Interval'.t Seq.t =
      collect_round_robin_non_decreasing ~skip_check batches
      |> Seq.flat_map (fun l -> CCList.to_seq l |> Seq.filter_map CCFun.id)
      |> normalize ~skip_filter_invalid:true ~skip_sort:true

    let merge_multi_seq_round_robin_non_decreasing ?(skip_check = false)
        (batches : Interval'.t Seq.t Seq.t) : Interval'.t Seq.t =
      batches
      |> CCList.of_seq
      |> merge_multi_list_round_robin_non_decreasing ~skip_check
  end

  let chunk ?(skip_check = false) ?(drop_partial = false) ~chunk_size
      (intervals : Interval'.t Seq.t) : Interval'.t Seq.t =
    let open Span in
    let rec aux intervals =
      match intervals () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((start, end_exc), rest) ->
        let size = end_exc - start in
        if size < chunk_size then
          if drop_partial then aux rest
          else fun () -> Seq.Cons ((start, end_exc), aux rest)
        else if size = chunk_size then fun () ->
          Seq.Cons ((start, end_exc), aux rest)
        else
          let chunk_end_exc = start + chunk_size in
          let rest () = Seq.Cons ((chunk_end_exc, end_exc), rest) in
          fun () -> Seq.Cons ((start, chunk_end_exc), aux rest)
    in
    if chunk_size < one_ns then invalid_arg "chunk"
    else
      intervals
      |> (fun s -> if skip_check then s else s |> Check.check_if_valid)
      |> aux
end

module Range = struct
  exception Modulo_is_invalid

  exception Range_is_invalid

  type 'a range =
    [ `Range_inc of 'a * 'a
    | `Range_exc of 'a * 'a
    ]

  let map ~(f_inc : 'a * 'a -> 'b * 'b) ~(f_exc : 'a * 'a -> 'b * 'b)
      (t : 'a range) : 'b range =
    match t with
    | `Range_inc (x, y) ->
      let x, y = f_inc (x, y) in
      `Range_inc (x, y)
    | `Range_exc (x, y) ->
      let x, y = f_exc (x, y) in
      `Range_exc (x, y)

  let int_range_of_range (type a) ~(to_int : a -> int) (x : a range) : int range
    =
    let f (x, y) = (to_int x, to_int y) in
    map ~f_inc:f ~f_exc:f x

  let int_inc_range_of_range (type a) ~(to_int : a -> int) (x : a range) :
    int * int =
    match x with
    | `Range_inc (x, y) -> (to_int x, to_int y)
    | `Range_exc (x, y) -> (to_int x, y |> to_int |> pred)

  let int_exc_range_of_range (type a) ~(to_int : a -> int) (x : a range) :
    int * int =
    match x with
    | `Range_inc (x, y) -> (to_int x, y |> to_int |> succ)
    | `Range_exc (x, y) -> (to_int x, to_int y)

  let inc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a)
      (x : a range) : a * a =
    match x with
    | `Range_inc (x, y) -> (x, y)
    | `Range_exc (x, y) -> (x, y |> to_int |> pred |> of_int)

  let exc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a)
      (x : a range) : a * a =
    match x with
    | `Range_inc (x, y) -> (x, y |> to_int |> succ |> of_int)
    | `Range_exc (x, y) -> (x, y)

  let timestamp_pair_of_int_pair (x, y) : timestamp * timestamp =
    (Span.make ~ns:x (), Span.make ~ns:y ())

  let int_pair_of_timestamp_pair
      (({ s = _; ns = x }, { s = _; ns = y }) : timestamp * timestamp) :
    int * int =
    (x, y)

  let join (type a) ~(to_int : a -> int) ~(of_int : int -> a) (x : a range)
      (y : a range) : a range option =
    let x = int_exc_range_of_range ~to_int x |> timestamp_pair_of_int_pair in
    let y = int_exc_range_of_range ~to_int y |> timestamp_pair_of_int_pair in
    Interval'.join x y
    |> CCOpt.map (fun (x, y) ->
        let open Span in
        `Range_exc (of_int x.ns, of_int y.ns))

  let is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
      (t : a range) : bool =
    match modulo with
    | None -> (
        match int_range_of_range ~to_int t with
        | `Range_inc (x, y) -> x <= y
        | `Range_exc (x, y) -> x <= y)
    | Some _ -> true

  module Flatten = struct
    let flatten_into_seq (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (t : a range) : a Seq.t =
      match t with
      | `Range_inc (start, end_inc) -> (
          let start = to_int start in
          let end_inc = to_int end_inc in
          if start <= end_inc then OSeq.(start -- end_inc) |> Seq.map of_int
          else
            match modulo with
            | None -> raise Range_is_invalid
            | Some modulo ->
              if modulo <= 0 then raise Modulo_is_invalid
              else
                OSeq.append OSeq.(start --^ modulo) OSeq.(0 -- end_inc)
                |> Seq.map of_int)
      | `Range_exc (start, end_exc) -> (
          let start = to_int start in
          let end_exc = to_int end_exc in
          if start <= end_exc then OSeq.(start --^ end_exc) |> Seq.map of_int
          else
            match modulo with
            | None -> raise Range_is_invalid
            | Some modulo ->
              if modulo <= 0 then raise Modulo_is_invalid
              else
                OSeq.append OSeq.(start --^ modulo) OSeq.(0 --^ end_exc)
                |> Seq.map of_int)

    let flatten_into_list (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (t : a range) : a list =
      flatten_into_seq ~modulo ~to_int ~of_int t |> CCList.of_seq
  end

  module type B = sig
    type t

    val modulo : int option

    val to_int : t -> int

    val of_int : int -> t
  end

  module type S = sig
    type t

    val int_range_of_range : t range -> int range

    val int_inc_range_of_range : t range -> int * int

    val int_exc_range_of_range : t range -> int * int

    val inc_range_of_range : t range -> t * t

    val exc_range_of_range : t range -> t * t

    val join : t range -> t range -> t range option

    val is_valid : t range -> bool

    module Flatten : sig
      val flatten_into_seq : t range -> t Seq.t

      val flatten_into_list : t range -> t list
    end
  end

  module Make (B : B) : S with type t := B.t = struct
    open B

    let int_range_of_range (x : t range) : int range =
      int_range_of_range ~to_int x

    let int_inc_range_of_range (x : t range) : int * int =
      int_inc_range_of_range ~to_int x

    let int_exc_range_of_range (x : t range) : int * int =
      int_exc_range_of_range ~to_int x

    let inc_range_of_range (x : t range) : t * t =
      inc_range_of_range ~to_int ~of_int x

    let exc_range_of_range (x : t range) : t * t =
      exc_range_of_range ~to_int ~of_int x

    let join (x : t range) (y : t range) : t range option =
      join ~to_int ~of_int x y

    let is_valid (x : t range) : bool = is_valid ~modulo ~to_int x

    module Flatten = struct
      let flatten_into_seq (t : t range) : t Seq.t =
        Flatten.flatten_into_seq ~modulo ~to_int ~of_int t

      let flatten_into_list (t : t range) : t list =
        Flatten.flatten_into_seq ~modulo ~to_int ~of_int t |> CCList.of_seq
    end
  end
end

module Range_utils = struct
  let option_range_get (x : 'a option Range.range) : 'a Range.range option =
    match x with
    | `Range_inc (x, y) -> (
        match (x, y) with
        | Some x, Some y -> Some (`Range_inc (x, y))
        | _, _ -> None)
    | `Range_exc (x, y) -> (
        match (x, y) with
        | Some x, Some y -> Some (`Range_exc (x, y))
        | _, _ -> None)
end

module Ranges = struct
  let normalize (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
      ~(to_int : a -> int) ~(of_int : int -> a) (s : a Range.range Seq.t) :
    a Range.range Seq.t =
    match modulo with
    | None ->
      s
      |> Seq.map (Range.int_exc_range_of_range ~to_int)
      |> Seq.map Range.timestamp_pair_of_int_pair
      |> Intervals.normalize ~skip_filter_invalid ~skip_filter_empty
        ~skip_sort
      |> Seq.map Range.int_pair_of_timestamp_pair
      |> Seq.map (fun (x, y) -> (of_int x, y |> pred |> of_int))
      |> Seq.map (fun (x, y) -> `Range_inc (x, y))
    | Some _ ->
      (* not sure what would be a reasonable normalization procedure when domain is a field *)
      s

  module Check = struct
    let seq_is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
        (s : a Range.range Seq.t) : bool =
      OSeq.for_all (Range.is_valid ~modulo ~to_int) s

    let list_is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
        (s : a Range.range list) : bool =
      List.for_all (Range.is_valid ~modulo ~to_int) s
  end

  module Flatten = struct
    let flatten (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (s : a Range.range Seq.t) : a Seq.t =
      Seq.flat_map (Range.Flatten.flatten_into_seq ~modulo ~to_int ~of_int) s

    let flatten_list (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (l : a Range.range list) : a list =
      l |> CCList.to_seq |> flatten ~modulo ~to_int ~of_int |> CCList.of_seq
  end

  module Of_seq = struct
    let range_seq_of_seq (type a) ?(skip_sort = false) ~(modulo : int option)
        ~(to_int : a -> int) ~(of_int : int -> a) (s : a Seq.t) :
      a Range.range Seq.t =
      s
      |> Seq.map (fun x -> `Range_inc (x, x))
      |> normalize ~skip_filter_invalid:true ~skip_filter_empty:true ~skip_sort
        ~modulo ~to_int ~of_int

    let range_list_of_seq (type a) ?(skip_sort = false) ~(modulo : int option)
        ~(to_int : a -> int) ~(of_int : int -> a) (s : a Seq.t) :
      a Range.range list =
      range_seq_of_seq ~skip_sort ~modulo ~to_int ~of_int s |> CCList.of_seq
  end

  module Of_list = struct
    let range_seq_of_list (type a) ?(skip_sort = false) ~(modulo : int option)
        ~(to_int : a -> int) ~(of_int : int -> a) (l : a list) :
      a Range.range Seq.t =
      CCList.to_seq l
      |> Of_seq.range_seq_of_seq ~skip_sort ~modulo ~to_int ~of_int

    let range_list_of_list (type a) ?(skip_sort = false) ~(modulo : int option)
        ~(to_int : a -> int) ~(of_int : int -> a) (l : a list) :
      a Range.range list =
      CCList.to_seq l
      |> Of_seq.range_seq_of_seq ~skip_sort ~modulo ~to_int ~of_int
      |> CCList.of_seq
  end

  module type S = sig
    type t

    val normalize :
      ?skip_filter_invalid:bool ->
      ?skip_filter_empty:bool ->
      ?skip_sort:bool ->
      t Range.range Seq.t ->
      t Range.range Seq.t

    module Check : sig
      val seq_is_valid : t Range.range Seq.t -> bool

      val list_is_valid : t Range.range list -> bool
    end

    module Flatten : sig
      val flatten : t Range.range Seq.t -> t Seq.t

      val flatten_list : t Range.range list -> t list
    end

    module Of_seq : sig
      val range_seq_of_seq : ?skip_sort:bool -> t Seq.t -> t Range.range Seq.t

      val range_list_of_seq : ?skip_sort:bool -> t Seq.t -> t Range.range list
    end

    module Of_list : sig
      val range_seq_of_list : ?skip_sort:bool -> t list -> t Range.range Seq.t

      val range_list_of_list : ?skip_sort:bool -> t list -> t Range.range list
    end
  end

  module Make (B : Range.B) : S with type t := B.t = struct
    open B

    let normalize ?skip_filter_invalid ?skip_filter_empty ?skip_sort
        (s : t Range.range Seq.t) =
      normalize ?skip_filter_invalid ?skip_filter_empty ?skip_sort ~modulo
        ~to_int ~of_int s

    module Check = struct
      let seq_is_valid s = Check.seq_is_valid ~modulo ~to_int s

      let list_is_valid l = Check.list_is_valid ~modulo ~to_int l
    end

    module Flatten = struct
      let flatten (s : t Range.range Seq.t) : t Seq.t =
        Flatten.flatten ~modulo ~to_int ~of_int s

      let flatten_list (l : t Range.range list) : t list =
        Flatten.flatten_list ~modulo ~to_int ~of_int l
    end

    module Of_seq = struct
      let range_seq_of_seq ?(skip_sort = false) (s : t Seq.t) :
        t Range.range Seq.t =
        Of_seq.range_seq_of_seq ~skip_sort ~modulo ~to_int ~of_int s

      let range_list_of_seq ?(skip_sort = false) (s : t Seq.t) :
        t Range.range list =
        Of_seq.range_list_of_seq ~skip_sort ~modulo ~to_int ~of_int s
    end

    module Of_list = struct
      let range_seq_of_list ?skip_sort (l : t list) : t Range.range Seq.t =
        CCList.to_seq l |> Of_seq.range_seq_of_seq ?skip_sort

      let range_list_of_list ?skip_sort (l : t list) : t Range.range list =
        CCList.to_seq l |> Of_seq.range_seq_of_seq ?skip_sort |> CCList.of_seq
    end
  end
end

module Second_ranges = Ranges.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Minute_ranges = Ranges.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Hour_ranges = Ranges.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

type hms = {
  hour : int;
  minute : int;
  second : int;
}

let make_hms ~hour ~minute ~second =
  if
    0 <= hour
    && hour <= 24
    && 0 <= minute
    && minute < 60
    && 0 <= second
    && second <= 60
  then
    let second = if second = 60 then 59 else second in
    if hour = 24 then
      if minute = 0 && second = 0 then
        Some { hour = 23; minute = 59; second = 59 }
      else None
    else Some { hour; minute; second }
  else None

let make_hms_exn ~hour ~minute ~second =
  match make_hms ~hour ~minute ~second with
  | Some x -> x
  | None -> invalid_arg "make_hms_exn"

let second_of_day_of_hms x =
  Duration.make ~hours:x.hour ~minutes:x.minute ~seconds:x.second ()
  |> Duration.to_span
  |> fun x -> Int64.to_int Span.(x.s)

let hms_of_second_of_day s =
  let ({ hours; minutes; seconds; _ } : Duration.t) =
    Duration.of_span (Span.make_small ~s ())
  in
  make_hms_exn ~hour:hours ~minute:minutes ~second:seconds

module Hms_ranges = Ranges.Make (struct
    type t = hms

    let modulo = None

    let to_int = second_of_day_of_hms

    let of_int = hms_of_second_of_day
  end)

module Weekday_tm_int_ranges = Ranges.Make (struct
    type t = int

    let modulo = Some 7

    let to_int x = x

    let of_int x = x
  end)

module Weekday_ranges = Ranges.Make (struct
    type t = weekday

    let modulo = Some 7

    let to_int = tm_int_of_weekday

    let of_int x = x |> weekday_of_tm_int |> CCOpt.get_exn
  end)

module Month_day_ranges = Ranges.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Month_tm_int_ranges = Ranges.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Month_ranges = Ranges.Make (struct
    type t = month

    let modulo = None

    let to_int = human_int_of_month

    let of_int x = x |> month_of_human_int |> CCOpt.get_exn
  end)

module Year_ranges = Ranges.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

let timestamp_now () : timestamp = Span.of_float @@ Unix.gettimeofday ()

let timestamp_min = Constants.timestamp_min

let timestamp_max = Constants.timestamp_max

let slice_valid_interval s =
  Intervals.Slice.slice ~skip_check:true ~start:timestamp_min
    ~end_exc:timestamp_max s

module Date_time' = struct
  type t = {
    year : int;
    month : month;
    day : int;
    hour : int;
    minute : int;
    second : int;
    ns : int;
    tz_info : tz_info;
  }

  let utc_tz_info : tz_info = (Time_zone.utc, Some Duration.zero)

  let dummy_tz_info = utc_tz_info

  let to_ptime_date_time_pretend_utc (x : t) : Ptime.date * Ptime.time =
    ( (x.year, human_int_of_month x.month, x.day),
      ((x.hour, x.minute, x.second), 0) )

  let of_ptime_date_time_pretend_utc
      (((year, month, day), ((hour, minute, second), _tz_offset_s)) :
         Ptime.date * Ptime.time) : t option =
    month_of_human_int month
    |> CCOpt.map (fun month ->
        {
          year;
          month;
          day;
          hour;
          minute;
          second;
          ns = 0;
          tz_info = utc_tz_info;
        })

  let to_timestamp_pretend_utc (x : t) : timestamp option =
    to_ptime_date_time_pretend_utc x
    |> Ptime.of_date_time
    |> CCOpt.map (fun t ->
        Span.make
          ~s:(Ptime_utils.timestamp_of_ptime t)
          ~ns:(x.ns mod Span.ns_count_in_s)
          ())

  let to_timestamp_precise_unsafe (x : t) : timestamp Time_zone.local_result =
    let open Span in
    match to_timestamp_pretend_utc x with
    | None -> `None
    | Some timestamp_local -> (
        match x.tz_info with
        | _tz, Some offset -> `Single (timestamp_local - Duration.to_span offset)
        | tz, None -> (
            match Time_zone.lookup_timestamp_local tz timestamp_local.s with
            | `None -> `None
            | `Single e -> `Single (timestamp_local - make_small ~s:e.offset ())
            | `Ambiguous (e1, e2) ->
              let x1 = timestamp_local - make_small ~s:e1.offset () in
              let x2 = timestamp_local - make_small ~s:e2.offset () in
              `Ambiguous (min x1 x2, max x1 x2)))

  type 'a local_result =
    [ `Single of 'a
    | `Ambiguous of 'a * 'a
    ]

  let min_of_local_result (r : 'a local_result) : 'a =
    match r with `Single x | `Ambiguous (x, _) -> x

  let max_of_local_result (r : 'a local_result) : 'a =
    match r with `Single x | `Ambiguous (_, x) -> x

  let to_timestamp x : timestamp local_result =
    match to_timestamp_precise_unsafe x with
    | `None -> failwith "Unexpected case"
    | `Single x -> `Single x
    | `Ambiguous (x, y) -> `Ambiguous (x, y)

  let to_timestamp_float x : float local_result =
    match to_timestamp_precise_unsafe x with
    | `None -> failwith "Unexpected case"
    | `Single x -> `Single (Span.to_float x)
    | `Ambiguous (x, y) -> `Ambiguous (Span.to_float x, Span.to_float y)

  let to_timestamp_single (x : t) : timestamp =
    match to_timestamp x with
    | `Single x -> x
    | `Ambiguous _ ->
      invalid_arg "to_timestamp_single: date time maps to two timestamps"

  let to_timestamp_float_single (x : t) : float =
    match to_timestamp x with
    | `Single x -> Span.to_float x
    | `Ambiguous _ ->
      invalid_arg
        "to_timestamp_precise_single: date time maps to two timestamps"

  let of_timestamp ?(tz_of_date_time = Time_zone_utils.get_local_tz_for_arg ())
      ({ s; ns } as x : timestamp) : t option =
    if not Span.(timestamp_min <= x && x <= timestamp_max) then None
    else
      match Time_zone.lookup_timestamp_utc tz_of_date_time s with
      | None -> None
      | Some entry -> (
          match Ptime_utils.ptime_of_timestamp s with
          | None -> None
          | Some x ->
            x
            |> Ptime.to_date_time ~tz_offset_s:entry.offset
            |> of_ptime_date_time_pretend_utc
            |> CCOpt.map (fun t ->
                {
                  t with
                  ns;
                  tz_info =
                    ( tz_of_date_time,
                      Some
                        (Duration.of_span
                           (Span.make_small ~s:entry.offset ())) );
                }))

  let of_timestamp_float ?tz_of_date_time (x : float) : t option =
    of_timestamp ?tz_of_date_time @@ Span.of_float x

  let is_leap_second (dt : t) = dt.ns >= Span.ns_count_in_s

  let adjust_ns_for_leap_second ~is_leap_second (dt : t) : t =
    if is_leap_second then { dt with ns = dt.ns + Span.ns_count_in_s } else dt

  let check_args_and_normalize_ns ~day ~hour ~minute ~second ~ns ~frac =
    if day < 1 || 31 < day then invalid_arg "day is out of range";
    if hour < 0 || 23 < hour then invalid_arg "hour is out of range";
    if minute < 0 || 59 < minute then invalid_arg "minute is out of range";
    if second < 0 || 60 < second then invalid_arg "second is out of range";
    if frac < 0. then invalid_arg "frac is negative";
    if ns < 0 then invalid_arg "ns is negative";
    let ns = ns + int_of_float (frac *. Span.ns_count_in_s_float) in
    if ns >= Span.ns_count_in_s then invalid_arg "ns is >= 10^9";
    ns

  let make ?(tz = Time_zone_utils.get_local_tz_for_arg ()) ?(ns = 0)
      ?(frac = 0.) ~year ~month ~day ~hour ~minute ~second () =
    let ns = check_args_and_normalize_ns ~day ~hour ~minute ~second ~ns ~frac in
    let is_leap_second = second = 60 in
    let second = if second = 60 then 59 else second in
    let dt =
      { year; month; day; hour; minute; second; ns; tz_info = (tz, None) }
    in
    (match to_timestamp_precise_unsafe dt with
     | `None -> None
     | `Single x -> Some (of_timestamp ~tz_of_date_time:tz x |> CCOpt.get_exn)
     | `Ambiguous _ -> Some dt)
    |> CCOpt.map (adjust_ns_for_leap_second ~is_leap_second)

  let make_exn ?tz ?ns ?frac ~year ~month ~day ~hour ~minute ~second () =
    match make ?tz ~year ~month ~day ~hour ~minute ~second ?ns ?frac () with
    | Some x -> x
    | None -> invalid_arg "make_exn"

  let make_unambiguous ?tz ?(ns = 0) ?(frac = 0.) ~year ~month ~day ~hour
      ~minute ~second ~tz_offset () =
    let ns = check_args_and_normalize_ns ~day ~hour ~minute ~second ~ns ~frac in
    let is_leap_second = second = 60 in
    let second = if second = 60 then 59 else second in
    let tz_info : tz_info option =
      match make_tz_info ?tz ~tz_offset () with
      | None -> None
      | Some ((tz, _) as tz_info) -> (
          match
            to_timestamp_pretend_utc
              {
                year;
                month;
                day;
                hour;
                minute;
                second;
                ns;
                tz_info = dummy_tz_info;
              }
          with
          | None -> None
          | Some { s = timestamp_local; ns = _ } -> (
              let tz_offset_s = Int64.to_int (Duration.to_span tz_offset).s in
              match Time_zone.lookup_timestamp_local tz timestamp_local with
              | `None -> None
              | `Single e ->
                if e.offset = tz_offset_s then Some tz_info else None
              | `Ambiguous (e1, e2) ->
                if e1.offset = tz_offset_s || e2.offset = tz_offset_s then
                  Some tz_info
                else None))
    in
    (match tz_info with
     | None -> None
     | Some tz_info -> (
         let dt = { year; month; day; hour; minute; second; ns; tz_info } in
         match to_timestamp_precise_unsafe dt with `None -> None | _ -> Some dt))
    |> CCOpt.map (adjust_ns_for_leap_second ~is_leap_second)

  let make_unambiguous_exn ?tz ?ns ?frac ~year ~month ~day ~hour ~minute ~second
      ~tz_offset () =
    let x =
      make_unambiguous ?tz ~year ~month ~day ~hour ~minute ~second ?ns ?frac
        ~tz_offset ()
    in
    match x with None -> invalid_arg "make_precise_exn" | Some x -> x

  let min_val =
    CCOpt.get_exn @@ of_timestamp ~tz_of_date_time:Time_zone.utc timestamp_min

  let max_val =
    CCOpt.get_exn @@ of_timestamp ~tz_of_date_time:Time_zone.utc timestamp_max

  let of_points
      ?(default_tz_info =
        CCOpt.get_exn
        @@ make_tz_info ~tz:(Time_zone_utils.get_local_tz_for_arg ()) ())
      ((pick, tz_info) : Points.t) : t option =
    match pick with
    | Points.YMDHMS { year; month; month_day; hour; minute; second } -> (
        let day_count = day_count_of_month ~year ~month in
        if abs month_day > day_count then None
        else
          let month_day =
            if month_day < 0 then day_count + month_day + 1 else month_day
          in
          match CCOpt.value ~default:default_tz_info tz_info with
          | tz, None ->
            make ~year ~month ~day:month_day ~hour ~minute ~second ~tz ()
          | tz, Some tz_offset ->
            make_unambiguous ~year ~month ~day:month_day ~hour ~minute ~second
              ~tz ~tz_offset ())
    | _ -> None

  let now ?tz_of_date_time () : t =
    timestamp_now () |> of_timestamp ?tz_of_date_time |> CCOpt.get_exn

  let to_weekday (x : t) : weekday =
    CCOpt.get_exn @@ weekday_of_month_day ~year:x.year ~month:x.month ~day:x.day

  let equal (x : t) (y : t) : bool =
    x.year = y.year
    && x.month = y.month
    && x.day = y.day
    && x.hour = y.hour
    && x.minute = y.minute
    && x.second = y.second
    && x.ns = y.ns
    && equal_tz_info x.tz_info y.tz_info

  let set_to_first_ns (x : t) : t = { x with ns = 0 }

  let set_to_last_ns (x : t) : t = { x with ns = Span.ns_count_in_s - 1 }

  let set_to_first_sec_ns (x : t) : t = { x with second = 0 } |> set_to_first_ns

  let set_to_last_sec_ns (x : t) : t = { x with second = 59 } |> set_to_last_ns

  let set_to_first_min_sec_ns (x : t) : t =
    { x with minute = 0 } |> set_to_first_sec_ns

  let set_to_last_min_sec_ns (x : t) : t =
    { x with minute = 59 } |> set_to_last_sec_ns

  let set_to_first_hour_min_sec_ns (x : t) : t =
    { x with hour = 0 } |> set_to_first_min_sec_ns

  let set_to_last_hour_min_sec_ns (x : t) : t =
    { x with hour = 23 } |> set_to_last_min_sec_ns

  let set_to_first_day_hour_min_sec_ns (x : t) : t =
    { x with day = 1 } |> set_to_first_hour_min_sec_ns

  let set_to_last_day_hour_min_sec_ns (x : t) : t =
    { x with day = day_count_of_month ~year:x.year ~month:x.month }
    |> set_to_last_hour_min_sec_ns

  let set_to_first_month_day_hour_min_sec_ns (x : t) : t =
    { x with month = `Jan } |> set_to_first_day_hour_min_sec_ns

  let set_to_last_month_day_hour_min_sec_ns (x : t) : t =
    { x with month = `Dec } |> set_to_last_day_hour_min_sec_ns
end

type sign_expr =
  | Pos
  | Neg

let equal_unary_op op1 op2 =
  match (op1, op2) with
  | Not, Not -> true
  (* | Drop_points n1, Drop_points n2 | Take_points n1, Take_points n2 -> n1 = n2 *)
  | Shift n1, Shift n2 | Lengthen n1, Lengthen n2 -> n1 = n2
  | With_tz tz1, With_tz tz2 -> Time_zone.equal tz1 tz2
  | _, _ -> false

let equal t1 t2 =
  let rec aux t1 t2 =
    match (t1, t2) with
    | Empty, Empty -> true
    | All, All -> true
    | Intervals s1, Intervals s2 -> OSeq.equal ~eq:( = ) s1 s2
    | Pattern p1, Pattern p2 -> Pattern.equal p1 p2
    | Unary_op (op1, t1), Unary_op (op2, t2) ->
      equal_unary_op op1 op2 && aux t1 t2
    | ( Bounded_intervals
          { pick = pick1; bound = b1; start = start1; end_exc = end_exc1 },
        Bounded_intervals
          { pick = pick2; bound = b2; start = start2; end_exc = end_exc2 } ) ->
      pick1 = pick2
      && b1 = b2
      && Points.equal start1 start2
      && Points.equal end_exc1 end_exc2
    | Inter_seq s1, Inter_seq s2 | Union_seq s1, Union_seq s2 ->
      OSeq.for_all2 aux s1 s2
    | Unchunk c1, Unchunk c2 -> aux_chunked c1 c2
    | _, _ -> false
  and aux_chunked c1 c2 =
    match (c1, c2) with
    | Unary_op_on_t (op1, t1), Unary_op_on_t (op2, t2) -> op1 = op2 && aux t1 t2
    | Unary_op_on_chunked (op1, c1), Unary_op_on_chunked (op2, c2) ->
      op1 = op2 && aux_chunked c1 c2
    | _, _ -> false
  in
  aux t1 t2

let chunk (chunking : chunking) (f : chunked -> chunked) t : t =
  match chunking with
  | `Disjoint_intervals ->
    Unchunk (f (Unary_op_on_t (Chunk_disjoint_interval, t)))
  | `By_duration duration ->
    if Duration.is_neg duration then invalid_arg "chunk: duration is negative";
    let chunk_size = Duration.to_span duration in
    if Span.(chunk_size < one_ns) then invalid_arg "chunk"
    else
      Unchunk
        (f
           (Unary_op_on_t
              (Chunk_by_duration { chunk_size; drop_partial = false }, t)))
  | `By_duration_drop_partial duration ->
    if Duration.is_neg duration then invalid_arg "chunk: duration is negative";
    let chunk_size = Duration.to_span duration in
    if Span.(chunk_size < one_ns) then invalid_arg "chunk"
    else
      Unchunk
        (f
           (Unary_op_on_t
              (Chunk_by_duration { chunk_size; drop_partial = true }, t)))
  | `At_year_boundary -> Unchunk (f (Unary_op_on_t (Chunk_at_year_boundary, t)))
  | `At_month_boundary ->
    Unchunk (f (Unary_op_on_t (Chunk_at_month_boundary, t)))

let chunk_again (chunking : chunking) chunked : chunked =
  match chunking with
  | `Disjoint_intervals ->
    Unary_op_on_chunked (Chunk_again Chunk_disjoint_interval, chunked)
  | `By_duration duration ->
    let chunk_size = Duration.to_span duration in
    if Span.(chunk_size < one_ns) then invalid_arg "chunk_again"
    else
      Unary_op_on_chunked
        ( Chunk_again
            (Chunk_by_duration
               {
                 chunk_size = Duration.to_span duration;
                 drop_partial = false;
               }),
          chunked )
  | `By_duration_drop_partial duration ->
    let chunk_size = Duration.to_span duration in
    if Span.(chunk_size < one_ns) then invalid_arg "chunk_again"
    else
      Unary_op_on_chunked
        ( Chunk_again
            (Chunk_by_duration
               { chunk_size = Duration.to_span duration; drop_partial = true }),
          chunked )
  | `At_year_boundary ->
    Unary_op_on_chunked (Chunk_again Chunk_at_year_boundary, chunked)
  | `At_month_boundary ->
    Unary_op_on_chunked (Chunk_again Chunk_at_year_boundary, chunked)

let shift (offset : Duration.t) (t : t) : t =
  Unary_op (Shift (Duration.to_span offset), t)

let lengthen (x : Duration.t) (t : t) : t =
  if Duration.is_neg x then invalid_arg "lengthen: duration is negative";
  Unary_op (Lengthen (Duration.to_span x), t)

let empty = Empty

let always = All

type inter_pattern_acc =
  | Uninitialized
  | Unsatisfiable
  | Some' of Pattern.t

let inter_seq (s : t Seq.t) : t =
  let flatten s =
    Seq.flat_map
      (fun x -> match x with Inter_seq s -> s | _ -> Seq.return x)
      s
  in
  let inter_patterns s =
    let patterns, rest =
      OSeq.partition (fun x -> match x with Pattern _ -> true | _ -> false) s
    in
    let pattern =
      Seq.fold_left
        (fun acc x ->
           match x with
           | Pattern pat -> (
               match acc with
               | Uninitialized -> Some' pat
               | Unsatisfiable -> Unsatisfiable
               | Some' acc -> (
                   match Pattern.inter acc pat with
                   | None -> Unsatisfiable
                   | Some pat -> Some' pat))
           | _ -> acc)
        Uninitialized patterns
    in
    match pattern with
    | Uninitialized -> Some rest
    | Unsatisfiable -> None
    | Some' pat -> Some (fun () -> Seq.Cons (Pattern pat, rest))
  in
  let s = flatten s in
  if OSeq.exists (fun x -> match x with Empty -> true | _ -> false) s then empty
  else match inter_patterns s with None -> empty | Some s -> Inter_seq s

let inter (l : t list) : t = inter_seq (CCList.to_seq l)

let union_seq (s : t Seq.t) : t =
  let flatten s =
    Seq.flat_map
      (fun x -> match x with Union_seq s -> s | _ -> Seq.return x)
      s
  in
  let s =
    s
    |> flatten
    |> Seq.filter (fun x -> match x with Empty -> false | _ -> true)
  in
  Union_seq s

let union (l : t list) : t = union_seq (CCList.to_seq l)

let first (c : chunked) : chunked = Unary_op_on_chunked (Take 1, c)

let take (n : int) (c : chunked) : chunked =
  if n < 0 then invalid_arg "take_n: n < 0" else Unary_op_on_chunked (Take n, c)

let take_nth (n : int) (c : chunked) : chunked =
  if n < 1 then invalid_arg "take_nth: n < 1"
  else Unary_op_on_chunked (Take_nth n, c)

let nth (n : int) (c : chunked) : chunked =
  if n < 0 then invalid_arg "nth: n < 0" else Unary_op_on_chunked (Nth n, c)

let drop (n : int) (c : chunked) : chunked =
  if n < 0 then invalid_arg "skip_n: n < 0" else Unary_op_on_chunked (Drop n, c)

let not (a : t) : t = Unary_op (Not, a)

let with_tz tz t = Unary_op (With_tz tz, t)

let pattern ?(years = []) ?(year_ranges = []) ?(months = [])
    ?(month_ranges = []) ?(days = []) ?(day_ranges = []) ?(weekdays = [])
    ?(weekday_ranges = []) ?(hours = []) ?(hour_ranges = []) ?(minutes = [])
    ?(minute_ranges = []) ?(seconds = []) ?(second_ranges = []) () : t =
  let years =
    try years @ Year_ranges.Flatten.flatten_list year_ranges
    with Range.Range_is_invalid -> invalid_arg "pattern: invalid year range"
  in
  let months =
    try months @ Month_ranges.Flatten.flatten_list month_ranges
    with Range.Range_is_invalid -> invalid_arg "pattern: invalid month range"
  in
  let month_days =
    try days @ Month_day_ranges.Flatten.flatten_list day_ranges
    with Range.Range_is_invalid -> invalid_arg "pattern: invalid day range"
  in
  let weekdays =
    try weekdays @ Weekday_ranges.Flatten.flatten_list weekday_ranges
    with Range.Range_is_invalid ->
      invalid_arg "pattern: invalid weekday range"
  in
  let hours =
    try hours @ Hour_ranges.Flatten.flatten_list hour_ranges
    with Range.Range_is_invalid -> invalid_arg "pattern: invalid hour range"
  in
  let minutes =
    try minutes @ Minute_ranges.Flatten.flatten_list minute_ranges
    with Range.Range_is_invalid -> invalid_arg "pattern: invalid minute range"
  in
  let seconds =
    try seconds @ Second_ranges.Flatten.flatten_list second_ranges
    with Range.Range_is_invalid -> invalid_arg "pattern: invalid second range"
  in
  match (years, months, month_days, weekdays, hours, minutes, seconds) with
  | [], [], [], [], [], [], [] -> All
  | _ ->
    if
      List.for_all
        (fun year -> Constants.min_year <= year && year <= Constants.max_year)
        years
      && List.for_all (fun x -> -31 <= x && x <= 31 && x <> 0) month_days
      && List.for_all (fun x -> 0 <= x && x < 24) hours
      && List.for_all (fun x -> 0 <= x && x < 60) minutes
      && List.for_all (fun x -> 0 <= x && x < 60) seconds
    then
      let years = Int_set.of_list years in
      let months = Month_set.of_list months in
      let month_days = Int_set.of_list month_days in
      let weekdays = Weekday_set.of_list weekdays in
      let hours = Int_set.of_list hours in
      let minutes = Int_set.of_list minutes in
      let seconds = Int_set.of_list seconds in
      Pattern
        {
          Pattern.years;
          months;
          month_days;
          weekdays;
          hours;
          minutes;
          seconds;
        }
    else invalid_arg "pattern"

let month_day_ranges_are_valid_strict ~safe_month_day_range_inc day_ranges =
  let safe_month_day_start, safe_month_day_end_inc = safe_month_day_range_inc in
  day_ranges
  |> CCList.to_seq
  |> Month_day_ranges.Flatten.flatten
  |> Seq.filter (fun mday -> mday <> 0)
  |> OSeq.for_all (fun mday ->
      safe_month_day_start <= mday && mday <= safe_month_day_end_inc)

let month_day_ranges_are_valid_relaxed day_range =
  month_day_ranges_are_valid_strict ~safe_month_day_range_inc:(-31, 31)
    day_range

let years years = pattern ~years ()

let months months = pattern ~months ()

let days days = pattern ~days ()

let weekdays weekdays = pattern ~weekdays ()

let hours hours = pattern ~hours ()

let minutes minutes = pattern ~minutes ()

let seconds seconds = pattern ~seconds ()

let year_ranges year_ranges = pattern ~year_ranges ()

let month_ranges month_ranges = pattern ~month_ranges ()

let day_ranges day_ranges = pattern ~day_ranges ()

let weekday_ranges weekday_ranges = pattern ~weekday_ranges ()

let hour_ranges hour_ranges = pattern ~hour_ranges ()

let minute_ranges minute_ranges = pattern ~minute_ranges ()

let second_ranges second_ranges = pattern ~second_ranges ()

let bounded_intervals pick (bound : Duration.t)
    ((pick_start, tz_info_start) as start : Points.t)
    ((pick_end_exc, tz_info_end_exc) as end_exc : Points.t) : t =
  if Duration.is_neg bound then
    invalid_arg "bounded_intervals: bound is negative";
  if Points.precision start < Points.precision end_exc then
    invalid_arg "bounded_intervals: start is less precise than end_exc"
  else if CCOpt.equal equal_tz_info tz_info_start tz_info_end_exc then
    match (pick_start, pick_end_exc) with
    | Points.(S second_start, S second_end_exc)
      when second_start = second_end_exc ->
      always
    | Points.(
        ( MS { minute = minute_start; second = second_start },
          MS { minute = minute_end_exc; second = second_end_exc } ))
      when minute_start = minute_end_exc && second_start = second_end_exc ->
      always
    | Points.(
        ( HMS { hour = hour_start; minute = minute_start; second = second_start },
          HMS
            {
              hour = hour_end_exc;
              minute = minute_end_exc;
              second = second_end_exc;
            } ))
      when hour_start = hour_end_exc
        && minute_start = minute_end_exc
        && second_start = second_end_exc ->
      always
    | _, _ ->
      Bounded_intervals
        { pick; bound = Duration.to_span bound; start; end_exc }
  else
    Bounded_intervals { pick; bound = Duration.to_span bound; start; end_exc }

let hms_intervals_exc (hms_a : hms) (hms_b : hms) : t =
  bounded_intervals `Whole (Duration.make ~days:1 ())
    (Points.make_exn ~hour:hms_a.hour ~minute:hms_a.minute ~second:hms_a.second
       ())
    (Points.make_exn ~hour:hms_b.hour ~minute:hms_b.minute ~second:hms_b.second
       ())

let hms_intervals_inc (hms_a : hms) (hms_b : hms) : t =
  let hms_b = hms_b |> second_of_day_of_hms |> succ |> hms_of_second_of_day in
  hms_intervals_exc hms_a hms_b

let sorted_interval_seq ?(skip_invalid : bool = false) (s : Interval'.t Seq.t) :
  t =
  let s =
    s
    |> Intervals.Filter.filter_empty
    |> (if skip_invalid then Intervals.Filter.filter_invalid
        else Intervals.Check.check_if_valid)
    |> Seq.filter_map (fun (x, y) ->
        match
          ( Date_time'.of_timestamp ~tz_of_date_time:Time_zone.utc x,
            Date_time'.of_timestamp ~tz_of_date_time:Time_zone.utc
              (Span.pred y) )
        with
        | Some _, Some _ -> Some (x, y)
        | _, _ -> if skip_invalid then None else raise Interval_is_invalid)
    |> Intervals.Check.check_if_sorted
    |> Intervals.normalize ~skip_filter_invalid:true ~skip_sort:true
    |> slice_valid_interval
  in
  match s () with Seq.Nil -> Empty | _ -> Intervals s

let sorted_intervals ?skip_invalid (l : Interval'.t list) : t =
  l |> CCList.to_seq |> sorted_interval_seq ?skip_invalid

let intervals ?(skip_invalid : bool = false) (l : Interval'.t list) : t =
  let s =
    l
    |> Intervals.Filter.filter_empty_list
    |> (if skip_invalid then Intervals.Filter.filter_invalid_list
        else Intervals.Check.check_if_valid_list)
    |> CCList.filter_map (fun (x, y) ->
        match
          ( Date_time'.of_timestamp ~tz_of_date_time:Time_zone.utc x,
            Date_time'.of_timestamp ~tz_of_date_time:Time_zone.utc
              (Span.pred y) )
        with
        | Some _, Some _ -> Some (x, y)
        | _, _ -> if skip_invalid then None else raise Interval_is_invalid)
    |> Intervals.Sort.sort_uniq_intervals_list
    |> CCList.to_seq
    |> Intervals.normalize ~skip_filter_invalid:true ~skip_sort:true
    |> slice_valid_interval
  in
  match s () with Seq.Nil -> Empty | _ -> Intervals s

let interval_seq ?skip_invalid (s : Interval'.t Seq.t) : t =
  s |> CCList.of_seq |> intervals ?skip_invalid

let interval_of_date_time date_time =
  let x = Date_time'.to_timestamp_single date_time in
  (x, Span.succ x)

let date_time_seq date_times =
  date_times |> Seq.map interval_of_date_time |> interval_seq

let date_times date_times = date_times |> CCList.to_seq |> date_time_seq

let sorted_date_time_seq date_times =
  date_times |> Seq.map interval_of_date_time |> sorted_interval_seq

let sorted_date_times date_times =
  date_times |> CCList.to_seq |> sorted_date_time_seq

let date_time date_time = date_times [ date_time ]

let interval_of_timestamp_precise ~skip_invalid x =
  match Date_time'.of_timestamp ~tz_of_date_time:Time_zone.utc x with
  | Some _ -> Some (x, Span.succ x)
  | None -> if skip_invalid then None else raise Invalid_timestamp

let timestamp_seq ?(skip_invalid = false) timestamps =
  timestamps
  |> Seq.filter_map (interval_of_timestamp_precise ~skip_invalid)
  |> interval_seq

let timestamps ?(skip_invalid = false) timestamps =
  timestamps
  |> CCList.filter_map (interval_of_timestamp_precise ~skip_invalid)
  |> intervals

let sorted_timestamp_seq ?(skip_invalid = false) timestamps =
  timestamps
  |> Seq.filter_map (interval_of_timestamp_precise ~skip_invalid)
  |> sorted_interval_seq

let sorted_timestamps ?(skip_invalid = false) timestamps =
  timestamps
  |> CCList.filter_map (interval_of_timestamp_precise ~skip_invalid)
  |> sorted_intervals

let timestamp x = timestamps [ x ]

let now () = timestamp (timestamp_now ())

let before_timestamp timestamp = intervals [ (timestamp_min, timestamp) ]

let since_timestamp timestamp = intervals [ (timestamp, timestamp_max) ]

let after_timestamp timestamp =
  intervals [ (Span.succ timestamp, timestamp_max) ]

let before dt =
  before_timestamp Date_time'.(to_timestamp dt |> min_of_local_result)

let since dt =
  since_timestamp Date_time'.(to_timestamp dt |> max_of_local_result)

let after dt =
  after_timestamp Date_time'.(to_timestamp dt |> max_of_local_result)

let nth_weekday_of_month (n : int) wday =
  let first_weekday_of_month wday =
    pattern ~day_ranges:[ `Range_inc (1, 7) ] ~weekdays:[ wday ] ()
  in
  let second_weekday_of_month wday =
    pattern ~day_ranges:[ `Range_inc (8, 14) ] ~weekdays:[ wday ] ()
  in
  let third_weekday_of_month wday =
    pattern ~day_ranges:[ `Range_inc (15, 21) ] ~weekdays:[ wday ] ()
  in
  let fourth_weekday_of_month wday =
    pattern ~day_ranges:[ `Range_inc (22, 28) ] ~weekdays:[ wday ] ()
  in
  let fifth_weekday_of_month wday =
    pattern ~days:[ 29; 30; 31 ] ~weekdays:[ wday ] ()
  in
  match n with
  | 0 -> invalid_arg "nth_weekday_of_month: n = 0"
  | 1 -> first_weekday_of_month wday
  | 2 -> second_weekday_of_month wday
  | 3 -> third_weekday_of_month wday
  | 4 -> fourth_weekday_of_month wday
  | 5 -> fifth_weekday_of_month wday
  | _ -> invalid_arg "nth_weekday_of_month: n > 5"

let full_string_of_weekday (wday : weekday) : string =
  match wday with
  | `Sun -> "Sunday"
  | `Mon -> "Monday"
  | `Tue -> "Tuesday"
  | `Wed -> "Wednesday"
  | `Thu -> "Thursday"
  | `Fri -> "Friday"
  | `Sat -> "Saturday"

let weekday_of_full_string s : weekday option =
  match s with
  | "Sunday" -> Some `Sun
  | "Monday" -> Some `Mon
  | "Tuesday" -> Some `Tue
  | "Wednesday" -> Some `Wed
  | "Thursday" -> Some `Thu
  | "Friday" -> Some `Fri
  | "Saturday" -> Some `Sat
  | _ -> None

let abbr_string_of_weekday (wday : weekday) : string =
  String.sub (full_string_of_weekday wday) 0 3

let weekday_of_abbr_string s : weekday option =
  match s with
  | "Sun" -> Some `Sun
  | "Mon" -> Some `Mon
  | "Tue" -> Some `Tue
  | "Wed" -> Some `Wed
  | "Thu" -> Some `Thu
  | "Fri" -> Some `Fri
  | "Sat" -> Some `Sat
  | _ -> None

let full_string_of_month (month : month) : string =
  match month with
  | `Jan -> "January"
  | `Feb -> "February"
  | `Mar -> "March"
  | `Apr -> "April"
  | `May -> "May"
  | `Jun -> "June"
  | `Jul -> "July"
  | `Aug -> "August"
  | `Sep -> "September"
  | `Oct -> "October"
  | `Nov -> "November"
  | `Dec -> "December"

let month_of_full_string s : month option =
  match s with
  | "January" -> Some `Jan
  | "February" -> Some `Feb
  | "March" -> Some `Mar
  | "April" -> Some `Apr
  | "May" -> Some `May
  | "June" -> Some `Jun
  | "July" -> Some `Jul
  | "August" -> Some `Aug
  | "September" -> Some `Sep
  | "October" -> Some `Oct
  | "November" -> Some `Nov
  | "December" -> Some `Dec
  | _ -> None

let abbr_string_of_month (month : month) : string =
  String.sub (full_string_of_month month) 0 3

let month_of_abbr_string s : month option =
  match s with
  | "Jan" -> Some `Jan
  | "Feb" -> Some `Feb
  | "Mar" -> Some `Mar
  | "Apr" -> Some `Apr
  | "May" -> Some `May
  | "Jun" -> Some `Jun
  | "Jul" -> Some `Jul
  | "Aug" -> Some `Aug
  | "Sep" -> Some `Sep
  | "Oct" -> Some `Oct
  | "Nov" -> Some `Nov
  | "Dec" -> Some `Dec
  | _ -> None
