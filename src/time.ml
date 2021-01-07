type tz_offset_s = int

type timestamp = int64

let tz_offset_s_utc = 0

exception Invalid_timestamp

exception Interval_is_invalid

exception Interval_is_empty

exception Intervals_are_not_sorted

exception Intervals_are_not_disjoint

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

type month =
  [ `Jan
  | `Feb
  | `Mar
  | `Apr
  | `May
  | `Jun
  | `Jul
  | `Aug
  | `Sep
  | `Oct
  | `Nov
  | `Dec
  ]

let first_mday = 1

let tm_year_offset = 1900

let tm_int_of_weekday (wday : weekday) : int =
  match wday with
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6

let weekday_of_tm_int (x : int) : (weekday, unit) result =
  match x with
  | 0 -> Ok `Sun
  | 1 -> Ok `Mon
  | 2 -> Ok `Tue
  | 3 -> Ok `Wed
  | 4 -> Ok `Thu
  | 5 -> Ok `Fri
  | 6 -> Ok `Sat
  | _ -> Error ()

let tm_int_of_month (month : month) : int =
  match month with
  | `Jan -> 0
  | `Feb -> 1
  | `Mar -> 2
  | `Apr -> 3
  | `May -> 4
  | `Jun -> 5
  | `Jul -> 6
  | `Aug -> 7
  | `Sep -> 8
  | `Oct -> 9
  | `Nov -> 10
  | `Dec -> 11

let month_of_tm_int (x : int) : (month, unit) result =
  match x with
  | 0 -> Ok `Jan
  | 1 -> Ok `Feb
  | 2 -> Ok `Mar
  | 3 -> Ok `Apr
  | 4 -> Ok `May
  | 5 -> Ok `Jun
  | 6 -> Ok `Jul
  | 7 -> Ok `Aug
  | 8 -> Ok `Sep
  | 9 -> Ok `Oct
  | 10 -> Ok `Nov
  | 11 -> Ok `Dec
  | _ -> Error ()

let human_int_of_month (month : month) : int = tm_int_of_month month + 1

let month_of_human_int (x : int) : (month, unit) result = month_of_tm_int (x - 1)

let compare_month (m1 : month) (m2 : month) : int =
  compare (tm_int_of_month m1) (tm_int_of_month m2)

module Month_set = struct
  include CCSet.Make (struct
      type t = month

      let compare = compare_month
    end)

  let to_seq x = x |> to_list |> CCList.to_seq
end

let compare_weekday (d1 : weekday) (d2 : weekday) : int =
  compare (tm_int_of_weekday d1) (tm_int_of_weekday d2)

module Weekday_set = struct
  include CCSet.Make (struct
      type t = weekday

      let compare = compare_weekday
    end)

  let to_seq x = x |> to_list |> CCList.to_seq
end

let is_leap_year ~year =
  assert (year >= 0);
  let divisible_by_4 = year mod 4 = 0 in
  let divisible_by_100 = year mod 100 = 0 in
  let divisible_by_400 = year mod 400 = 0 in
  divisible_by_4 && ((not divisible_by_100) || divisible_by_400)

let day_count_of_year ~year = if is_leap_year ~year then 366 else 365

let day_count_of_month ~year ~(month : month) =
  match month with
  | `Jan -> 31
  | `Feb -> if is_leap_year ~year then 29 else 28
  | `Mar -> 31
  | `Apr -> 30
  | `May -> 31
  | `Jun -> 30
  | `Jul -> 31
  | `Aug -> 31
  | `Sep -> 30
  | `Oct -> 31
  | `Nov -> 30
  | `Dec -> 31

let weekday_of_month_day ~(year : int) ~(month : month) ~(mday : int) :
  (weekday, unit) result =
  match Ptime.(of_date (year, human_int_of_month month, mday)) with
  | None -> Error ()
  | Some wday -> Ok (Ptime.weekday wday)

module Interval = struct
  type t = int64 * int64

  let lt (x1, y1) (x2, y2) =
    (* lexicographical order *)
    x1 < x2 || (x1 = x2 && y1 < y2)

  let le x y = lt x y || x = y

  let gt x y = lt y x

  let ge x y = le y x

  let compare x y = if lt x y then -1 else if x = y then 0 else 1

  module Check = struct
    let is_valid ((start, end_exc) : t) : bool = start <= end_exc

    let is_not_empty ((start, end_exc) : t) : bool = start <> end_exc

    let check_if_valid (x : t) : t =
      if is_valid x then x else raise Interval_is_invalid

    let check_if_not_empty (x : t) : t =
      if is_not_empty x then x else raise Interval_is_empty
  end

  let join (ts1 : t) (ts2 : t) : t option =
    let aux (start1, end_exc1) (start2, end_exc2) =
      if start2 <= end_exc1 then Some (start1, max end_exc1 end_exc2) else None
    in
    let start1, end_exc1 = Check.check_if_valid ts1 in
    let start2, end_exc2 = Check.check_if_valid ts2 in
    if start1 <= start2 then aux (start1, end_exc1) (start2, end_exc2)
    else aux (start2, end_exc2) (start1, end_exc1)

  let overlap_of_a_over_b ~(a : t) ~(b : t) : t option * t option * t option =
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
  open Int64_utils

  module Check = struct
    let check_if_valid (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      Seq.map Interval.Check.check_if_valid intervals

    let check_if_valid_list (intervals : Interval.t list) : Interval.t list =
      List.map Interval.Check.check_if_valid intervals

    let check_if_not_empty (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      Seq.map Interval.Check.check_if_not_empty intervals

    let check_if_sorted (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      Seq_utils.check_if_f_holds_for_immediate_neighbors ~f:Interval.le
        ~f_exn:(fun _ _ -> Intervals_are_not_sorted)
        intervals

    let check_if_sorted_rev (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      Seq_utils.check_if_f_holds_for_immediate_neighbors ~f:Interval.ge
        ~f_exn:(fun _ _ -> Intervals_are_not_sorted)
        intervals

    let check_if_disjoint (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      Seq_utils.check_if_f_holds_for_immediate_neighbors
        ~f:(fun x y ->
            match Interval.overlap_of_a_over_b ~a:y ~b:x with
            | None, None, None | Some _, None, None | None, None, Some _ -> true
            | _ -> false)
        ~f_exn:(fun _ _ -> Intervals_are_not_disjoint)
        intervals

    let check_if_normalized (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      intervals
      |> check_if_valid
      |> check_if_not_empty
      |> check_if_sorted
      |> check_if_disjoint
  end

  module Filter = struct
    let filter_invalid (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      Seq.filter Interval.Check.is_valid intervals

    let filter_invalid_list (intervals : Interval.t list) : Interval.t list =
      List.filter Interval.Check.is_valid intervals

    let filter_empty (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      Seq.filter Interval.Check.is_not_empty intervals

    let filter_empty_list (intervals : Interval.t list) : Interval.t list =
      List.filter Interval.Check.is_not_empty intervals
  end

  module Sort = struct
    let sort_intervals_list ?(skip_check = false) (intervals : Interval.t list)
      : Interval.t list =
      intervals
      |> (fun l ->
          if skip_check then l
          else l |> CCList.to_seq |> Check.check_if_valid |> CCList.of_seq)
      |> List.sort Interval.compare

    let sort_uniq_intervals_list ?(skip_check = false)
        (intervals : Interval.t list) : Interval.t list =
      intervals
      |> (fun l ->
          if skip_check then l
          else l |> CCList.to_seq |> Check.check_if_valid |> CCList.of_seq)
      |> List.sort_uniq Interval.compare

    let sort_uniq_intervals ?(skip_check = false) (intervals : Interval.t Seq.t)
      : Interval.t Seq.t =
      intervals
      |> (fun s -> if skip_check then s else Check.check_if_valid s)
      |> CCList.of_seq
      |> List.sort_uniq Interval.compare
      |> CCList.to_seq

    let sort_intervals ?(skip_check = false) (intervals : Interval.t Seq.t) :
      Interval.t Seq.t =
      intervals
      |> (fun s -> if skip_check then s else Check.check_if_valid s)
      |> CCList.of_seq
      |> List.sort Interval.compare
      |> CCList.to_seq
  end

  module Join_internal = struct
    let join (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      let rec aux cur intervals =
        match intervals () with
        | Seq.Nil -> Seq.return cur
        | Seq.Cons ((start, end_exc), rest) -> (
            match Interval.join cur (start, end_exc) with
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
    let slice_start ~start (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      let rec aux start intervals =
        match intervals () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons ((ts_start, ts_end_exc), rest) ->
          if start <= ts_start then
            (* entire time slot is after start, do nothing *)
            intervals
          else if ts_start < start && start < ts_end_exc then
            (* time slot spans across the start mark, split time slot *)
            fun () -> Seq.Cons ((start, ts_end_exc), rest)
          else
            (* time slot is before start mark, move to next time slot *)
            aux start rest
      in
      aux start intervals

    let slice_end_exc ~end_exc (intervals : Interval.t Seq.t) : Interval.t Seq.t
      =
      let rec aux end_exc intervals =
        match intervals () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons ((ts_start, ts_end_exc), rest) ->
          if end_exc <= ts_start then
            (* entire time slot is after end_exc mark, drop everything *)
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

  let relative_complement ?(skip_check = false) ~(not_mem_of : Interval.t Seq.t)
      (mem_of : Interval.t Seq.t) : Interval.t Seq.t =
    let rec aux mem_of not_mem_of =
      match (mem_of (), not_mem_of ()) with
      | Seq.Nil, _ -> Seq.empty
      | _, Seq.Nil -> mem_of
      | ( Seq.Cons (mem_of_ts, mem_of_rest),
          Seq.Cons (not_mem_of_ts, not_mem_of_rest) ) -> (
          let mem_of () = Seq.Cons (mem_of_ts, mem_of_rest) in
          let not_mem_of () = Seq.Cons (not_mem_of_ts, not_mem_of_rest) in
          match Interval.overlap_of_a_over_b ~a:mem_of_ts ~b:not_mem_of_ts with
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
      (intervals : Interval.t Seq.t) : Interval.t Seq.t =
    relative_complement ~skip_check ~not_mem_of:intervals
      (Seq.return (start, end_exc))

  module Inter = struct
    let inter ?(skip_check = false) (intervals1 : Interval.t Seq.t)
        (intervals2 : Interval.t Seq.t) : Interval.t Seq.t =
      let rec aux intervals1 intervals2 : Interval.t Seq.t =
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
        (interval_batches : Interval.t Seq.t Seq.t) : Interval.t Seq.t =
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
    let merge ?(skip_check = false) (intervals1 : Interval.t Seq.t)
        (intervals2 : Interval.t Seq.t) : Interval.t Seq.t =
      let rec aux intervals1 intervals2 =
        match (intervals1 (), intervals2 ()) with
        | Seq.Nil, s | s, Seq.Nil -> fun () -> s
        | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
          if Interval.le x1 x2 then fun () ->
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
        (interval_batches : Interval.t Seq.t Seq.t) : Interval.t Seq.t =
      Seq.fold_left
        (fun acc intervals -> merge ~skip_check acc intervals)
        Seq.empty interval_batches
  end

  module Union = struct
    let union ?(skip_check = false) (intervals1 : Interval.t Seq.t)
        (intervals2 : Interval.t Seq.t) : Interval.t Seq.t =
      Merge.merge ~skip_check intervals1 intervals2
      |> normalize ~skip_filter_invalid:true ~skip_sort:true

    let union_multi_seq ?(skip_check = false)
        (interval_batches : Interval.t Seq.t Seq.t) : Interval.t Seq.t =
      Seq.fold_left
        (fun acc intervals -> union ~skip_check acc intervals)
        Seq.empty interval_batches
  end

  module Round_robin = struct
    let collect_round_robin_non_decreasing ?(skip_check = false)
        (batches : Interval.t Seq.t list) : Interval.t option list Seq.t =
      batches
      |> List.map (fun s ->
          if skip_check then s
          else s |> Check.check_if_valid |> Check.check_if_sorted)
      |> Seq_utils.collect_round_robin ~f_le:Interval.le

    let merge_multi_list_round_robin_non_decreasing ?(skip_check = false)
        (batches : Interval.t Seq.t list) : Interval.t Seq.t =
      collect_round_robin_non_decreasing ~skip_check batches
      |> Seq.flat_map (fun l -> CCList.to_seq l |> Seq.filter_map CCFun.id)
      |> normalize ~skip_filter_invalid:true ~skip_sort:true

    let merge_multi_seq_round_robin_non_decreasing ?(skip_check = false)
        (batches : Interval.t Seq.t Seq.t) : Interval.t Seq.t =
      batches
      |> CCList.of_seq
      |> merge_multi_list_round_robin_non_decreasing ~skip_check
  end

  let chunk ?(skip_check = false) ?(drop_partial = false) ~chunk_size
      (intervals : Interval.t Seq.t) : Interval.t Seq.t =
    let rec aux intervals =
      match intervals () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((start, end_exc), rest) ->
        let size = end_exc -^ start in
        if size < chunk_size then
          if drop_partial then aux rest
          else fun () -> Seq.Cons ((start, end_exc), aux rest)
        else if size = chunk_size then fun () ->
          Seq.Cons ((start, end_exc), aux rest)
        else
          let chunk_end_exc = start +^ chunk_size in
          let rest () = Seq.Cons ((chunk_end_exc, end_exc), rest) in
          fun () -> Seq.Cons ((start, chunk_end_exc), aux rest)
    in
    if chunk_size < 1L then invalid_arg "chunk"
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

  let int64_range_of_range (type a) ~(to_int64 : a -> int64) (x : a range) :
    int64 range =
    let f (x, y) = (to_int64 x, to_int64 y) in
    map ~f_inc:f ~f_exc:f x

  let int64_inc_range_of_range (type a) ~(to_int64 : a -> int64) (x : a range) :
    int64 * int64 =
    match x with
    | `Range_inc (x, y) -> (to_int64 x, to_int64 y)
    | `Range_exc (x, y) -> (to_int64 x, y |> to_int64 |> Int64.pred)

  let int64_exc_range_of_range (type a) ~(to_int64 : a -> int64) (x : a range) :
    int64 * int64 =
    match x with
    | `Range_inc (x, y) -> (to_int64 x, y |> to_int64 |> Int64.succ)
    | `Range_exc (x, y) -> (to_int64 x, to_int64 y)

  let inc_range_of_range (type a) ~(to_int64 : a -> int64)
      ~(of_int64 : int64 -> a) (x : a range) : a * a =
    match x with
    | `Range_inc (x, y) -> (x, y)
    | `Range_exc (x, y) -> (x, y |> to_int64 |> Int64.pred |> of_int64)

  let exc_range_of_range (type a) ~(to_int64 : a -> int64)
      ~(of_int64 : int64 -> a) (x : a range) : a * a =
    match x with
    | `Range_inc (x, y) -> (x, y |> to_int64 |> Int64.succ |> of_int64)
    | `Range_exc (x, y) -> (x, y)

  let join (type a) ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a)
      (x : a range) (y : a range) : a range option =
    let x = int64_exc_range_of_range ~to_int64 x in
    let y = int64_exc_range_of_range ~to_int64 y in
    Interval.join x y
    |> CCOpt.map (fun (x, y) -> `Range_exc (of_int64 x, of_int64 y))

  let is_valid (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
      (t : a range) : bool =
    match modulo with
    | None -> (
        match int64_range_of_range ~to_int64 t with
        | `Range_inc (x, y) -> x <= y
        | `Range_exc (x, y) -> x <= y)
    | Some _ -> true

  module Flatten = struct
    let flatten_into_seq (type a) ~(modulo : int64 option)
        ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (t : a range) :
      a Seq.t =
      match t with
      | `Range_inc (start, end_inc) -> (
          let start = to_int64 start in
          let end_inc = to_int64 end_inc in
          if start <= end_inc then
            Seq_utils.a_to_b_inc_int64 ~a:start ~b:end_inc |> Seq.map of_int64
          else
            match modulo with
            | None -> raise Range_is_invalid
            | Some modulo ->
              if modulo <= 0L then raise Modulo_is_invalid
              else
                OSeq.append
                  (Seq_utils.a_to_b_exc_int64 ~a:start ~b:modulo)
                  (Seq_utils.a_to_b_inc_int64 ~a:0L ~b:end_inc)
                |> Seq.map of_int64)
      | `Range_exc (start, end_exc) -> (
          let start = to_int64 start in
          let end_exc = to_int64 end_exc in
          if start <= end_exc then
            Seq_utils.a_to_b_exc_int64 ~a:start ~b:end_exc |> Seq.map of_int64
          else
            match modulo with
            | None -> raise Range_is_invalid
            | Some modulo ->
              if modulo <= 0L then raise Modulo_is_invalid
              else
                OSeq.append
                  (Seq_utils.a_to_b_exc_int64 ~a:start ~b:modulo)
                  (Seq_utils.a_to_b_exc_int64 ~a:0L ~b:end_exc)
                |> Seq.map of_int64)

    let flatten_into_list (type a) ~(modulo : int64 option)
        ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (t : a range) : a list
      =
      flatten_into_seq ~modulo ~to_int64 ~of_int64 t |> CCList.of_seq
  end

  module type B = sig
    type t

    val modulo : int64 option

    val to_int64 : t -> int64

    val of_int64 : int64 -> t
  end

  module type S = sig
    type t

    val int64_range_of_range : t range -> int64 range

    val int64_inc_range_of_range : t range -> int64 * int64

    val int64_exc_range_of_range : t range -> int64 * int64

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

    let int64_range_of_range (x : t range) : int64 range =
      int64_range_of_range ~to_int64 x

    let int64_inc_range_of_range (x : t range) : int64 * int64 =
      int64_inc_range_of_range ~to_int64 x

    let int64_exc_range_of_range (x : t range) : int64 * int64 =
      int64_exc_range_of_range ~to_int64 x

    let inc_range_of_range (x : t range) : t * t =
      inc_range_of_range ~to_int64 ~of_int64 x

    let exc_range_of_range (x : t range) : t * t =
      exc_range_of_range ~to_int64 ~of_int64 x

    let join (x : t range) (y : t range) : t range option =
      join ~to_int64 ~of_int64 x y

    let is_valid (x : t range) : bool = is_valid ~modulo ~to_int64 x

    module Flatten = struct
      let flatten_into_seq (t : t range) : t Seq.t =
        Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t

      let flatten_into_list (t : t range) : t list =
        Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t |> CCList.of_seq
    end
  end
end

module Range_utils = struct
  let result_range_get (x : ('a, 'b) result Range.range) : 'a Range.range option
    =
    match x with
    | `Range_inc (x, y) -> (
        match (x, y) with
        | Ok x, Ok y -> Some (`Range_inc (x, y))
        | _, _ -> None)
    | `Range_exc (x, y) -> (
        match (x, y) with
        | Ok x, Ok y -> Some (`Range_exc (x, y))
        | _, _ -> None)
end

module Range_small = struct
  let int_range_of_range (type a) ~(to_int : a -> int) (x : a Range.range) :
    int Range.range =
    let f (x, y) = (to_int x, to_int y) in
    Range.map ~f_inc:f ~f_exc:f x

  let int_exc_range_of_range (type a) ~(to_int : a -> int) (x : a Range.range) :
    int * int =
    match x with
    | `Range_inc (x, y) -> (to_int x, y |> to_int |> CCInt.succ)
    | `Range_exc (x, y) -> (to_int x, to_int y)

  let inc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a)
      (x : a Range.range) : a * a =
    match x with
    | `Range_inc (x, y) -> (x, y)
    | `Range_exc (x, y) -> (x, y |> to_int |> CCInt.pred |> of_int)

  let exc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a)
      (x : a Range.range) : a * a =
    match x with
    | `Range_inc (x, y) -> (x, y |> to_int |> CCInt.succ |> of_int)
    | `Range_exc (x, y) -> (x, y)

  let join (type a) ~(to_int : a -> int) ~(of_int : int -> a)
      (x : a Range.range) (y : a Range.range) : a Range.range option =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    Range.join ~to_int64 ~of_int64 x y

  module Flatten = struct
    let flatten_into_seq (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (t : a Range.range) : a Seq.t =
      let modulo = CCOpt.map Int64.of_int modulo in
      let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
      let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
      Range.Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t

    let flatten_into_list (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (t : a Range.range) : a list =
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

    val int_range_of_range : t Range.range -> int Range.range

    val int_exc_range_of_range : t Range.range -> int * int

    val inc_range_of_range : t Range.range -> t * t

    val exc_range_of_range : t Range.range -> t * t

    val join : t Range.range -> t Range.range -> t Range.range option

    module Flatten : sig
      val flatten_into_seq : t Range.range -> t Seq.t

      val flatten_into_list : t Range.range -> t list
    end
  end

  module Make (B : B) : S with type t := B.t = struct
    open B

    let int_range_of_range (x : t Range.range) : int Range.range =
      int_range_of_range ~to_int x

    let int_exc_range_of_range (x : t Range.range) : int * int =
      int_exc_range_of_range ~to_int x

    let inc_range_of_range (x : t Range.range) : t * t =
      inc_range_of_range ~to_int ~of_int x

    let exc_range_of_range (x : t Range.range) : t * t =
      exc_range_of_range ~to_int ~of_int x

    let join (x : t Range.range) (y : t Range.range) : t Range.range option =
      join ~to_int ~of_int x y

    module Flatten = struct
      let flatten_into_seq (t : t Range.range) : t Seq.t =
        Flatten.flatten_into_seq ~modulo ~to_int ~of_int t

      let flatten_into_list (t : t Range.range) : t list =
        Flatten.flatten_into_seq ~modulo ~to_int ~of_int t |> CCList.of_seq
    end
  end
end

module Ranges = struct
  let normalize (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int64 option)
      ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a)
      (s : a Range.range Seq.t) : a Range.range Seq.t =
    match modulo with
    | None ->
      s
      |> Seq.map (Range.int64_exc_range_of_range ~to_int64)
      |> Intervals.normalize ~skip_filter_invalid ~skip_filter_empty
        ~skip_sort
      |> Seq.map (fun (x, y) -> (of_int64 x, y |> Int64.pred |> of_int64))
      |> Seq.map (fun (x, y) -> `Range_inc (x, y))
    | Some _ ->
      (* not sure what would be a reasonable normalization procedure when domain is a field *)
      s

  module Check = struct
    let seq_is_valid (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
        (s : a Range.range Seq.t) : bool =
      OSeq.for_all (Range.is_valid ~modulo ~to_int64) s

    let list_is_valid (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
        (s : a Range.range list) : bool =
      List.for_all (Range.is_valid ~modulo ~to_int64) s
  end

  module Flatten = struct
    let flatten (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
        ~(of_int64 : int64 -> a) (s : a Range.range Seq.t) : a Seq.t =
      Seq.flat_map
        (Range.Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64)
        s

    let flatten_list (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
        ~(of_int64 : int64 -> a) (l : a Range.range list) : a list =
      l |> CCList.to_seq |> flatten ~modulo ~to_int64 ~of_int64 |> CCList.of_seq
  end

  module Of_seq = struct
    let range_seq_of_seq (type a) ?(skip_sort = false) ~(modulo : int64 option)
        ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (s : a Seq.t) :
      a Range.range Seq.t =
      s
      |> Seq.map (fun x -> `Range_inc (x, x))
      |> normalize ~skip_filter_invalid:true ~skip_filter_empty:true ~skip_sort
        ~modulo ~to_int64 ~of_int64

    let range_list_of_seq (type a) ?(skip_sort = false) ~(modulo : int64 option)
        ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (s : a Seq.t) :
      a Range.range list =
      range_seq_of_seq ~skip_sort ~modulo ~to_int64 ~of_int64 s |> CCList.of_seq
  end

  module Of_list = struct
    let range_seq_of_list (type a) ?(skip_sort = false) ~(modulo : int64 option)
        ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (l : a list) :
      a Range.range Seq.t =
      CCList.to_seq l
      |> Of_seq.range_seq_of_seq ~skip_sort ~modulo ~to_int64 ~of_int64

    let range_list_of_list (type a) ?(skip_sort = false)
        ~(modulo : int64 option) ~(to_int64 : a -> int64)
        ~(of_int64 : int64 -> a) (l : a list) : a Range.range list =
      CCList.to_seq l
      |> Of_seq.range_seq_of_seq ~skip_sort ~modulo ~to_int64 ~of_int64
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

    let normalize ?(skip_filter_invalid = false) ?(skip_filter_empty = false)
        ?(skip_sort = false) (s : t Range.range Seq.t) =
      normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
        ~to_int64 ~of_int64 s

    module Check = struct
      let seq_is_valid s = Check.seq_is_valid ~modulo ~to_int64 s

      let list_is_valid l = Check.list_is_valid ~modulo ~to_int64 l
    end

    module Flatten = struct
      let flatten (s : t Range.range Seq.t) : t Seq.t =
        Flatten.flatten ~modulo ~to_int64 ~of_int64 s

      let flatten_list (l : t Range.range list) : t list =
        Flatten.flatten_list ~modulo ~to_int64 ~of_int64 l
    end

    module Of_seq = struct
      let range_seq_of_seq ?(skip_sort = false) (s : t Seq.t) :
        t Range.range Seq.t =
        Of_seq.range_seq_of_seq ~skip_sort ~modulo ~to_int64 ~of_int64 s

      let range_list_of_seq ?(skip_sort = false) (s : t Seq.t) :
        t Range.range list =
        Of_seq.range_list_of_seq ~skip_sort ~modulo ~to_int64 ~of_int64 s
    end

    module Of_list = struct
      let range_seq_of_list ?(skip_sort = false) (l : t list) :
        t Range.range Seq.t =
        CCList.to_seq l |> Of_seq.range_seq_of_seq ~skip_sort

      let range_list_of_list ?(skip_sort = false) (l : t list) :
        t Range.range list =
        CCList.to_seq l |> Of_seq.range_seq_of_seq ~skip_sort |> CCList.of_seq
    end
  end
end

module Ranges_small = struct
  let normalize (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
      ~(to_int : a -> int) ~(of_int : int -> a) (s : a Range.range Seq.t) :
    a Range.range Seq.t =
    let modulo = CCOpt.map Int64.of_int modulo in
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    Ranges.normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
      ~to_int64 ~of_int64 s

  module Check = struct
    let seq_is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
        (s : a Range.range Seq.t) : bool =
      let modulo = CCOpt.map Int64.of_int modulo in
      let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
      Ranges.Check.seq_is_valid ~modulo ~to_int64 s

    let list_is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
        (l : a Range.range list) : bool =
      let modulo = CCOpt.map Int64.of_int modulo in
      let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
      Ranges.Check.list_is_valid ~modulo ~to_int64 l
  end

  module Flatten = struct
    let flatten (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (s : a Range.range Seq.t) : a Seq.t =
      let modulo = CCOpt.map Int64.of_int modulo in
      let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
      let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
      Ranges.Flatten.flatten ~modulo ~to_int64 ~of_int64 s

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

  module Make (B : Range_small.B) : Ranges.S with type t := B.t = struct
    open B

    let normalize ?(skip_filter_invalid = false) ?(skip_filter_empty = false)
        ?(skip_sort = false) (s : t Range.range Seq.t) =
      normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
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
      let range_seq_of_list ?(skip_sort = false) (l : t list) :
        t Range.range Seq.t =
        CCList.to_seq l |> Of_seq.range_seq_of_seq ~skip_sort

      let range_list_of_list ?(skip_sort = false) (l : t list) :
        t Range.range list =
        CCList.to_seq l |> Of_seq.range_seq_of_seq ~skip_sort |> CCList.of_seq
    end
  end
end

module Second_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Minute_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Hour_ranges = Ranges_small.Make (struct
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
    && hour < 24
    && 0 <= minute
    && minute < 60
    && 0 <= second
    && second < 60
  then Ok { hour; minute; second }
  else Error ()

let make_hms_exn ~hour ~minute ~second =
  match make_hms ~hour ~minute ~second with
  | Ok x -> x
  | Error () -> invalid_arg "make_hms_exn"

let second_of_day_of_hms x =
  Duration.make ~hours:x.hour ~minutes:x.minute ~seconds:x.second ()
  |> Duration.to_seconds
  |> Int64.to_int

let hms_of_second_of_day x =
  let ({ hours; minutes; seconds; _ } : Duration.t) =
    x |> Int64.of_int |> Duration.of_seconds
  in
  CCResult.get_exn @@ make_hms ~hour:hours ~minute:minutes ~second:seconds

module Hms_ranges = Ranges_small.Make (struct
    type t = hms

    let modulo = None

    let to_int = second_of_day_of_hms

    let of_int = hms_of_second_of_day
  end)

module Weekday_tm_int_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = Some 7

    let to_int x = x

    let of_int x = x
  end)

module Weekday_ranges = Ranges_small.Make (struct
    type t = weekday

    let modulo = Some 7

    let to_int = tm_int_of_weekday

    let of_int x = x |> weekday_of_tm_int |> CCResult.get_exn
  end)

module Month_day_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Month_tm_int_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Month_ranges = Ranges_small.Make (struct
    type t = month

    let modulo = None

    let to_int = human_int_of_month

    let of_int x = x |> month_of_human_int |> CCResult.get_exn
  end)

module Year_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

let cur_timestamp () : int64 = Unix.time () |> Int64.of_float

let min_timestamp = Constants.min_timestamp

let max_timestamp = Constants.max_timestamp

module Date_time' = struct
  type tz_info =
    [ `Tz_only of Time_zone.t
    | `Tz_offset_s_only of int
    | `Tz_and_tz_offset_s of Time_zone.t * int
    ]

  type t = {
    year : int;
    month : month;
    day : int;
    hour : int;
    minute : int;
    second : int;
    tz_info : tz_info;
  }

  let utc_tz_info = `Tz_and_tz_offset_s (Time_zone.utc, 0)

  let dummy_tz_info = utc_tz_info

  let to_ptime_date_time_pretend_utc (x : t) : Ptime.date * Ptime.time =
    ( (x.year, human_int_of_month x.month, x.day),
      ((x.hour, x.minute, x.second), 0) )

  let of_ptime_date_time_pretend_utc
      (((year, month, day), ((hour, minute, second), _tz_offset_s)) :
         Ptime.date * Ptime.time) : (t, unit) result =
    match month_of_human_int month with
    | Ok month ->
      Ok { year; month; day; hour; minute; second; tz_info = utc_tz_info }
    | Error () -> Error ()

  let min_of_timestamp_local_result r : int64 option =
    match r with `None -> None | `Single x | `Ambiguous (x, _) -> Some x

  let max_of_timestamp_local_result r : int64 option =
    match r with `None -> None | `Single x | `Ambiguous (_, x) -> Some x

  let to_timestamp_pretend_utc (x : t) : timestamp =
    to_ptime_date_time_pretend_utc x
    |> Ptime.of_date_time
    |> CCOpt.get_exn
    |> Ptime.to_float_s
    |> Int64.of_float

  let to_timestamp (x : t) : timestamp Time_zone.local_result =
    let timestamp_local = to_timestamp_pretend_utc x in
    match x.tz_info with
    | `Tz_offset_s_only offset | `Tz_and_tz_offset_s (_, offset) ->
      `Single (Int64.sub timestamp_local (Int64.of_int offset))
    | `Tz_only tz -> (
        match Time_zone.lookup_timestamp_local tz timestamp_local with
        | `None -> `None
        | `Single e -> `Single (Int64.sub timestamp_local (Int64.of_int e.offset))
        | `Ambiguous (e1, e2) ->
          let x1 = Int64.sub timestamp_local (Int64.of_int e1.offset) in
          let x2 = Int64.sub timestamp_local (Int64.of_int e2.offset) in
          `Ambiguous (min x1 x2, max x1 x2))

  let to_timestamp_single (x : t) : timestamp =
    match to_timestamp x with
    | `None ->
      invalid_arg
        "to_timestamp_single: date time does not map to any timestamp"
    | `Single x -> x
    | `Ambiguous _ ->
      invalid_arg "to_timestamp_single: date time maps to two timestamps"

  let of_timestamp ?(tz_of_date_time = Time_zone.utc) (x : int64) :
    (t, unit) result =
    match Time_zone.lookup_timestamp_utc tz_of_date_time x with
    | None -> Error ()
    | Some entry -> (
        match Ptime.of_float_s (Int64.to_float x) with
        | None -> Error ()
        | Some x ->
          x
          |> Ptime.to_date_time ~tz_offset_s:entry.offset
          |> of_ptime_date_time_pretend_utc
          |> CCResult.map (fun t ->
              {
                t with
                tz_info =
                  `Tz_and_tz_offset_s (tz_of_date_time, entry.offset);
              }))

  let make ~year ~month ~day ~hour ~minute ~second ~tz =
    let dt =
      { year; month; day; hour; minute; second; tz_info = `Tz_only tz }
    in
    match to_timestamp dt with
    | `None -> Error ()
    | `Single x -> Ok (of_timestamp ~tz_of_date_time:tz x |> CCResult.get_exn)
    | `Ambiguous _ -> Ok dt

  let make_exn ~year ~month ~day ~hour ~minute ~second ~tz =
    match make ~year ~month ~day ~hour ~minute ~second ~tz with
    | Ok x -> x
    | Error () -> invalid_arg "make_exn"

  let make_precise ?tz ~year ~month ~day ~hour ~minute ~second ~tz_offset_s () =
    let tz_info : (tz_info, unit) result =
      match tz with
      | None -> Ok (`Tz_offset_s_only tz_offset_s)
      | Some tz ->
        if Time_zone.offset_is_recorded tz_offset_s tz then
          let timestamp_local =
            to_timestamp_pretend_utc
              {
                year;
                month;
                day;
                hour;
                minute;
                second;
                tz_info = dummy_tz_info;
              }
          in
          match Time_zone.lookup_timestamp_local tz timestamp_local with
          | `None -> Error ()
          | `Single e ->
            if e.offset = tz_offset_s then
              Ok (`Tz_and_tz_offset_s (tz, tz_offset_s))
            else Error ()
          | `Ambiguous (e1, e2) ->
            if e1.offset = tz_offset_s || e2.offset = tz_offset_s then
              Ok (`Tz_and_tz_offset_s (tz, tz_offset_s))
            else Error ()
        else Error ()
    in
    match tz_info with
    | Error () -> Error ()
    | Ok tz_info -> (
        let dt = { year; month; day; hour; minute; second; tz_info } in
        match to_timestamp dt with `None -> Error () | _ -> Ok dt)

  let make_precise_exn ?tz ~year ~month ~day ~hour ~minute ~second ~tz_offset_s
      () =
    let x =
      match tz with
      | None ->
        make_precise ~year ~month ~day ~hour ~minute ~second ~tz_offset_s ()
      | Some tz ->
        make_precise ~tz ~year ~month ~day ~hour ~minute ~second ~tz_offset_s
          ()
    in
    match x with Error () -> invalid_arg "make_precise_exn" | Ok x -> x

  let min = CCResult.get_exn @@ of_timestamp min_timestamp

  let max = CCResult.get_exn @@ of_timestamp max_timestamp

  let cur ?(tz_of_date_time = Time_zone.utc) () : (t, unit) result =
    cur_timestamp () |> of_timestamp ~tz_of_date_time

  let equal (x : t) (y : t) : bool =
    x.year = y.year
    && x.month = y.month
    && x.day = y.day
    && x.hour = y.hour
    && x.minute = y.minute
    && x.second = y.second
    &&
    match (x.tz_info, y.tz_info) with
    | `Tz_only x, `Tz_only y -> Time_zone.equal x y
    | `Tz_offset_s_only x, `Tz_offset_s_only y -> x = y
    | ( `Tz_and_tz_offset_s (tz_x, tz_offset_s_x),
        `Tz_and_tz_offset_s (tz_y, tz_offset_s_y) ) ->
      Time_zone.equal tz_x tz_y && tz_offset_s_x = tz_offset_s_y
    | _ -> false

  let set_to_first_sec (x : t) : t = { x with second = 0 }

  let set_to_last_sec (x : t) : t = { x with second = 59 }

  let set_to_first_min_sec (x : t) : t =
    { x with minute = 0 } |> set_to_first_sec

  let set_to_last_min_sec (x : t) : t =
    { x with minute = 59 } |> set_to_last_sec

  let set_to_first_hour_min_sec (x : t) : t =
    { x with hour = 0 } |> set_to_first_min_sec

  let set_to_last_hour_min_sec (x : t) : t =
    { x with hour = 23 } |> set_to_last_min_sec

  let set_to_first_day_hour_min_sec (x : t) : t =
    { x with day = 1 } |> set_to_first_hour_min_sec

  let set_to_last_day_hour_min_sec (x : t) : t =
    { x with day = day_count_of_month ~year:x.year ~month:x.month }
    |> set_to_last_hour_min_sec

  let set_to_first_month_day_hour_min_sec (x : t) : t =
    { x with month = `Jan } |> set_to_first_day_hour_min_sec

  let set_to_last_month_day_hour_min_sec (x : t) : t =
    { x with month = `Dec } |> set_to_last_day_hour_min_sec
end

module Check = struct
  let timestamp_is_valid (x : int64) : bool =
    match Date_time'.of_timestamp x with Ok _ -> true | Error () -> false

  let second_is_valid ~(second : int) : bool = 0 <= second && second < 60

  let minute_second_is_valid ~(minute : int) ~(second : int) : bool =
    0 <= minute && minute < 60 && second_is_valid ~second

  let hour_minute_second_is_valid ~(hour : int) ~(minute : int) ~(second : int)
    : bool =
    (0 <= hour && hour < 24) && minute_second_is_valid ~minute ~second
end

module Pattern = struct
  type t = {
    years : Int_set.t;
    months : Month_set.t;
    month_days : Int_set.t;
    weekdays : Weekday_set.t;
    hours : Int_set.t;
    minutes : Int_set.t;
    seconds : Int_set.t;
  }

  let equal p1 p2 =
    Int_set.equal p1.years p2.years
    && Month_set.equal p1.months p2.months
    && Int_set.equal p1.month_days p2.month_days
    && Weekday_set.equal p1.weekdays p2.weekdays
    && Int_set.equal p1.hours p2.hours
    && Int_set.equal p1.minutes p2.minutes
    && Int_set.equal p1.seconds p2.seconds

  type error =
    | Invalid_years of Int_set.t
    | Invalid_month_days of Int_set.t
    | Invalid_hours of Int_set.t
    | Invalid_minutes of Int_set.t
    | Invalid_seconds of Int_set.t

  module Check = struct
    let check_pattern (x : t) : (unit, error) result =
      let invalid_years = Int_set.filter (fun x -> x < 0 || 9999 < x) x.years in
      let invalid_month_days =
        Int_set.filter (fun x -> x < 1 || 31 < x) x.month_days
      in
      let invalid_hours = Int_set.filter (fun x -> x < 0 || 23 < x) x.hours in
      let invalid_minutes =
        Int_set.filter (fun x -> x < 0 || 59 < x) x.minutes
      in
      let invalid_seconds =
        Int_set.filter (fun x -> x < 0 || 59 < x) x.seconds
      in
      if Int_set.is_empty invalid_years then
        if Int_set.is_empty invalid_month_days then
          if Int_set.is_empty invalid_hours then
            if Int_set.is_empty invalid_minutes then
              if Int_set.is_empty invalid_seconds then Ok ()
              else Error (Invalid_seconds invalid_seconds)
            else Error (Invalid_minutes invalid_minutes)
          else Error (Invalid_hours invalid_hours)
        else Error (Invalid_month_days invalid_month_days)
      else Error (Invalid_years invalid_years)
  end

  let union p1 p2 =
    let union_sets (type a) ~(is_empty : a -> bool) ~(union : a -> a -> a)
        ~(empty : a) (a : a) (b : a) =
      if is_empty a || is_empty b then empty else union a b
    in
    let union_int_sets a b =
      union_sets ~is_empty:Int_set.is_empty ~union:Int_set.union
        ~empty:Int_set.empty a b
    in
    let union_month_sets a b =
      union_sets ~is_empty:Month_set.is_empty ~union:Month_set.union
        ~empty:Month_set.empty a b
    in
    let union_weekday_sets a b =
      union_sets ~is_empty:Weekday_set.is_empty ~union:Weekday_set.union
        ~empty:Weekday_set.empty a b
    in
    {
      years = union_int_sets p1.years p2.years;
      months = union_month_sets p1.months p2.months;
      month_days = union_int_sets p1.month_days p2.month_days;
      weekdays = union_weekday_sets p1.weekdays p2.weekdays;
      hours = union_int_sets p1.hours p2.hours;
      minutes = union_int_sets p1.minutes p2.minutes;
      seconds = union_int_sets p1.seconds p2.seconds;
    }

  let inter p1 p2 =
    let inter_sets (type a) ~(is_empty : a -> bool) ~(inter : a -> a -> a)
        (a : a) (b : a) =
      if is_empty a then Some b
      else if is_empty b then Some a
      else
        let s = inter a b in
        if is_empty s then None else Some s
    in
    let inter_int_sets a b =
      inter_sets ~is_empty:Int_set.is_empty ~inter:Int_set.inter a b
    in
    let inter_month_sets a b =
      inter_sets ~is_empty:Month_set.is_empty ~inter:Month_set.inter a b
    in
    let inter_weekday_sets a b =
      inter_sets ~is_empty:Weekday_set.is_empty ~inter:Weekday_set.inter a b
    in
    match inter_int_sets p1.years p2.years with
    | None -> None
    | Some years -> (
        match inter_month_sets p1.months p2.months with
        | None -> None
        | Some months -> (
            match inter_int_sets p1.month_days p2.month_days with
            | None -> None
            | Some month_days -> (
                match inter_weekday_sets p1.weekdays p2.weekdays with
                | None -> None
                | Some weekdays -> (
                    match inter_int_sets p1.hours p2.hours with
                    | None -> None
                    | Some hours -> (
                        match inter_int_sets p1.minutes p2.minutes with
                        | None -> None
                        | Some minutes -> (
                            match inter_int_sets p1.seconds p2.seconds with
                            | None -> None
                            | Some seconds ->
                              Some
                                {
                                  years;
                                  months;
                                  month_days;
                                  weekdays;
                                  hours;
                                  minutes;
                                  seconds;
                                }))))))
end

type sign_expr =
  | Pos
  | Neg

type unary_op =
  | Not
  | Drop_points of int
  | Take_points of int
  | Shift of int64
  | Lengthen of int64
  | With_tz of Time_zone.t

type search_space = Interval.t list

let default_search_space_start = min_timestamp

let default_search_space_end_exc = max_timestamp

let default_search_space : search_space =
  [ (default_search_space_start, default_search_space_end_exc) ]

type chunked_unary_op_on_t =
  | Chunk_disjoint_interval
  | Chunk_at_year_boundary
  | Chunk_at_month_boundary
  | Chunk_by_duration of {
      chunk_size : int64;
      drop_partial : bool;
    }

type chunked_unary_op_on_chunked =
  | Drop of int
  | Take of int
  | Take_nth of int
  | Nth of int
  | Chunk_again of chunked_unary_op_on_t

type t =
  | Empty
  | All
  | Timestamp_interval_seq of search_space * (int64 * int64) Seq.t
  | Pattern of search_space * Pattern.t
  | Unary_op of search_space * unary_op * t
  | Interval_inc of search_space * timestamp * timestamp
  | Interval_exc of search_space * timestamp * timestamp
  | Round_robin_pick_list of search_space * t list
  | Inter_seq of search_space * t Seq.t
  | Union_seq of search_space * t Seq.t
  | After of search_space * int64 * t * t
  | Between_inc of search_space * int64 * t * t
  | Between_exc of search_space * int64 * t * t
  | Unchunk of search_space * chunked

and chunked =
  | Unary_op_on_t of chunked_unary_op_on_t * t
  | Unary_op_on_chunked of chunked_unary_op_on_chunked * chunked

let equal_unary_op op1 op2 =
  match (op1, op2) with
  | Not, Not -> true
  | Drop_points n1, Drop_points n2 | Take_points n1, Take_points n2 -> n1 = n2
  | Shift n1, Shift n2 | Lengthen n1, Lengthen n2 -> n1 = n2
  | With_tz tz1, With_tz tz2 -> Time_zone.name tz1 = Time_zone.name tz2
  | _, _ -> false

let equal t1 t2 =
  let rec aux t1 t2 =
    match (t1, t2) with
    | Empty, Empty -> true
    | All, All -> true
    | Timestamp_interval_seq (_, s1), Timestamp_interval_seq (_, s2) ->
      OSeq.equal ~eq:( = ) s1 s2
    | Pattern (_, p1), Pattern (_, p2) -> Pattern.equal p1 p2
    | Unary_op (_, op1, t1), Unary_op (_, op2, t2) ->
      equal_unary_op op1 op2 && aux t1 t2
    | Interval_inc (_, x11, x12), Interval_inc (_, x21, x22)
    | Interval_exc (_, x11, x12), Interval_exc (_, x21, x22) ->
      x11 = x21 && x12 = x22
    | After (_, b1, x11, x12), After (_, b2, x21, x22)
    | Between_inc (_, b1, x11, x12), Between_inc (_, b2, x21, x22)
    | Between_exc (_, b1, x11, x12), Between_exc (_, b2, x21, x22) ->
      b1 = b2 && aux x11 x21 && aux x12 x22
    | Round_robin_pick_list (_, l1), Round_robin_pick_list (_, l2) ->
      List.for_all2 aux l1 l2
    | Inter_seq (_, s1), Inter_seq (_, s2) | Union_seq (_, s1), Union_seq (_, s2)
      ->
      OSeq.for_all2 aux s1 s2
    | Unchunk (_, c1), Unchunk (_, c2) -> aux_chunked c1 c2
    | _, _ -> false
  and aux_chunked c1 c2 =
    match (c1, c2) with
    | Unary_op_on_t (op1, t1), Unary_op_on_t (op2, t2) -> op1 = op2 && aux t1 t2
    | Unary_op_on_chunked (op1, c1), Unary_op_on_chunked (op2, c2) ->
      op1 = op2 && aux_chunked c1 c2
    | _, _ -> false
  in
  aux t1 t2

type chunking =
  [ `Disjoint_intervals
  | `By_duration of Duration.t
  | `By_duration_drop_partial of Duration.t
  | `At_year_boundary
  | `At_month_boundary
  ]

let chunk (chunking : chunking) (f : chunked -> chunked) t : t =
  match chunking with
  | `Disjoint_intervals ->
    Unchunk
      (default_search_space, f (Unary_op_on_t (Chunk_disjoint_interval, t)))
  | `By_duration duration ->
    let chunk_size = Duration.to_seconds duration in
    if chunk_size < 1L then invalid_arg "chunk"
    else
      Unchunk
        ( default_search_space,
          f
            (Unary_op_on_t
               (Chunk_by_duration { chunk_size; drop_partial = false }, t)) )
  | `By_duration_drop_partial duration ->
    let chunk_size = Duration.to_seconds duration in
    if chunk_size < 1L then invalid_arg "chunk"
    else
      Unchunk
        ( default_search_space,
          f
            (Unary_op_on_t
               (Chunk_by_duration { chunk_size; drop_partial = true }, t)) )
  | `At_year_boundary ->
    Unchunk
      (default_search_space, f (Unary_op_on_t (Chunk_at_year_boundary, t)))
  | `At_month_boundary ->
    Unchunk
      (default_search_space, f (Unary_op_on_t (Chunk_at_month_boundary, t)))

let chunk_again (chunking : chunking) chunked : chunked =
  match chunking with
  | `Disjoint_intervals ->
    Unary_op_on_chunked (Chunk_again Chunk_disjoint_interval, chunked)
  | `By_duration duration ->
    let chunk_size = Duration.to_seconds duration in
    if chunk_size < 1L then invalid_arg "chunk_again"
    else
      Unary_op_on_chunked
        ( Chunk_again
            (Chunk_by_duration
               {
                 chunk_size = Duration.to_seconds duration;
                 drop_partial = false;
               }),
          chunked )
  | `By_duration_drop_partial duration ->
    let chunk_size = Duration.to_seconds duration in
    if chunk_size < 1L then invalid_arg "chunk_again"
    else
      Unary_op_on_chunked
        ( Chunk_again
            (Chunk_by_duration
               {
                 chunk_size = Duration.to_seconds duration;
                 drop_partial = true;
               }),
          chunked )
  | `At_year_boundary ->
    Unary_op_on_chunked (Chunk_again Chunk_at_year_boundary, chunked)
  | `At_month_boundary ->
    Unary_op_on_chunked (Chunk_again Chunk_at_year_boundary, chunked)

let shift (offset : Duration.t) (t : t) : t =
  Unary_op (default_search_space, Shift (Duration.to_seconds offset), t)

let lengthen (x : Duration.t) (t : t) : t =
  Unary_op (default_search_space, Lengthen (Duration.to_seconds x), t)

let empty = Empty

let always = All

type inter_pattern_acc =
  | Uninitialized
  | Unsatisfiable
  | Some' of Pattern.t

let inter_seq (s : t Seq.t) : t =
  let flatten s =
    Seq.flat_map
      (fun x -> match x with Inter_seq (_, s) -> s | _ -> Seq.return x)
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
           | Pattern (_, pat) -> (
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
    | Some' pat ->
      Some (fun () -> Seq.Cons (Pattern (default_search_space, pat), rest))
  in
  let s = flatten s in
  if OSeq.exists (fun x -> match x with Empty -> true | _ -> false) s then empty
  else
    match inter_patterns s with
    | None -> empty
    | Some s -> Inter_seq (default_search_space, s)

let inter (l : t list) : t = inter_seq (CCList.to_seq l)

let union_seq (s : t Seq.t) : t =
  let flatten s =
    Seq.flat_map
      (fun x -> match x with Union_seq (_, s) -> s | _ -> Seq.return x)
      s
  in
  let s =
    s
    |> flatten
    |> Seq.filter (fun x -> match x with Empty -> false | _ -> true)
  in
  Union_seq (default_search_space, s)

let union (l : t list) : t = union_seq (CCList.to_seq l)

let round_robin_pick (l : t list) : t =
  Round_robin_pick_list (default_search_space, l)

let first_point (a : t) : t = Unary_op (default_search_space, Take_points 1, a)

let take_points (n : int) (t : t) : t =
  if n < 0 then invalid_arg "take_n_points: n < 0"
  else Unary_op (default_search_space, Take_points n, t)

let drop_points (n : int) (t : t) : t =
  if n < 0 then invalid_arg "drop_n_points: n < 0"
  else Unary_op (default_search_space, Drop_points n, t)

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

let interval_inc (a : timestamp) (b : timestamp) : t =
  match Date_time'.of_timestamp a with
  | Error () -> invalid_arg "interval_inc: invalid timestamp"
  | Ok _ -> (
      match Date_time'.of_timestamp b with
      | Error () -> invalid_arg "interval_inc: invalid timestamp"
      | Ok _ ->
        if a <= b then Interval_inc (default_search_space, a, b)
        else invalid_arg "interval_inc: a > b")

let interval_exc (a : timestamp) (b : timestamp) : t =
  match Date_time'.of_timestamp a with
  | Error () -> invalid_arg "interval_exc: invalid timestamp"
  | Ok _ -> (
      match Date_time'.of_timestamp b with
      | Error () -> invalid_arg "interval_exc: invalid timestamp"
      | Ok _ ->
        if a <= b then Interval_exc (default_search_space, a, b)
        else invalid_arg "interval_exc: a > b")

let interval_dt_inc (a : Date_time'.t) (b : Date_time'.t) : t =
  let a =
    CCOpt.get_exn Date_time'.(min_of_timestamp_local_result @@ to_timestamp a)
  in
  let b =
    CCOpt.get_exn Date_time'.(max_of_timestamp_local_result @@ to_timestamp b)
  in
  if a <= b then Interval_inc (default_search_space, a, b)
  else invalid_arg "interval_dt_inc: a > b"

let interval_dt_exc (a : Date_time'.t) (b : Date_time'.t) : t =
  let a =
    CCOpt.get_exn Date_time'.(min_of_timestamp_local_result @@ to_timestamp a)
  in
  let b =
    CCOpt.get_exn Date_time'.(max_of_timestamp_local_result @@ to_timestamp b)
  in
  if a <= b then Interval_exc (default_search_space, a, b)
  else invalid_arg "interval_dt_exc: a > b"

let not (a : t) : t = Unary_op (default_search_space, Not, a)

let with_tz offset t = Unary_op (default_search_space, With_tz offset, t)

(* let safe_month_day_range_inc ~years ~months =
 *   let contains_non_leap_year =
 *     years
 *     |> CCList.to_seq
 *     |> Year_ranges.Flatten.flatten
 *     |> OSeq.exists (fun year -> not (is_leap_year ~year))
 *   in
 *   let leap_year = 2020 in
 *   let non_leap_year = 2019 in
 *   let rec aux start end_inc months =
 *     match months () with
 *     | Seq.Nil -> (start, end_inc)
 *     | Seq.Cons (month, rest) ->
 *       let count =
 *         match month with
 *         | `Feb ->
 *           if contains_non_leap_year then
 *             day_count_of_month ~year:non_leap_year ~month
 *           else day_count_of_month ~year:leap_year ~month
 *         | _ -> day_count_of_month ~year:leap_year ~month
 *       in
 *       aux (max (-count) start) (min count end_inc) rest
 *   in
 *   aux (-31) 31 (Month_ranges.Flatten.flatten @@ CCList.to_seq @@ months) *)

let pattern ?(years = []) ?(year_ranges = []) ?(months = [])
    ?(month_ranges = []) ?(month_days = []) ?(month_day_ranges = [])
    ?(weekdays = []) ?(weekday_ranges = []) ?(hours = []) ?(hour_ranges = [])
    ?(minutes = []) ?(minute_ranges = []) ?(seconds = []) ?(second_ranges = [])
    () : t =
  let years = years @ Year_ranges.Flatten.flatten_list year_ranges in
  let months = months @ Month_ranges.Flatten.flatten_list month_ranges in
  let month_days =
    month_days @ Month_day_ranges.Flatten.flatten_list month_day_ranges
  in
  let weekdays =
    weekdays @ Weekday_ranges.Flatten.flatten_list weekday_ranges
  in
  let hours = hours @ Hour_ranges.Flatten.flatten_list hour_ranges in
  let minutes = minutes @ Minute_ranges.Flatten.flatten_list minute_ranges in
  let seconds = seconds @ Second_ranges.Flatten.flatten_list second_ranges in
  match (years, months, month_days, weekdays, hours, minutes, seconds) with
  | [], [], [], [], [], [], [] -> All
  | _ ->
    if
      List.for_all
        (fun year ->
           Date_time'.min.year <= year && year <= Date_time'.max.year)
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
        ( default_search_space,
          {
            Pattern.years;
            months;
            month_days;
            weekdays;
            hours;
            minutes;
            seconds;
          } )
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

let month_days month_days = pattern ~month_days ()

let weekdays weekdays = pattern ~weekdays ()

let hours hours = pattern ~hours ()

let minutes minutes = pattern ~minutes ()

let seconds seconds = pattern ~seconds ()

let after (bound : Duration.t) (t1 : t) (t2 : t) : t =
  After (default_search_space, Duration.to_seconds bound, t1, t2)

let between_inc (bound : Duration.t) (t1 : t) (t2 : t) : t =
  Between_inc (default_search_space, Duration.to_seconds bound, t1, t2)

let between_exc (bound : Duration.t) (t1 : t) (t2 : t) : t =
  Between_exc (default_search_space, Duration.to_seconds bound, t1, t2)

(* let hms_interval_exc (hms_a : hms) (hms_b : hms) : t =
 *   let a = second_of_day_of_hms hms_a in
 *   let b = second_of_day_of_hms hms_b in
 *   if a = b then empty
 *   else
 *     let gap_in_seconds =
 *       if a < b then Int64.of_int (b - a)
 *       else
 *         Int64.sub
 *           (Duration.make ~days:1 () |> CCResult.get_ok |> Duration.to_seconds)
 *           (Int64.of_int (a - b))
 *     in
 *     let gap_in_seconds_minus_one = Int64.pred gap_in_seconds in
 *     let gap_to_use =
 *       gap_in_seconds_minus_one |> Duration.of_seconds |> CCResult.get_ok
 *     in
 *     lengthen gap_to_use
 *       (pattern ~hours:[ hms_a.hour ] ~minutes:[ hms_a.minute ]
 *          ~seconds:[ hms_a.second ] ()) *)

let hms_interval_exc (hms_a : hms) (hms_b : hms) : t =
  between_exc (Duration.make ~days:1 ())
    (pattern ~hours:[ hms_a.hour ] ~minutes:[ hms_a.minute ]
       ~seconds:[ hms_a.second ] ())
    (pattern ~hours:[ hms_b.hour ] ~minutes:[ hms_b.minute ]
       ~seconds:[ hms_b.second ] ())

let hms_interval_inc (hms_a : hms) (hms_b : hms) : t =
  let hms_b = hms_b |> second_of_day_of_hms |> succ |> hms_of_second_of_day in
  hms_interval_exc hms_a hms_b

let of_hms_intervals (s : (hms * hms) Seq.t) : t =
  s |> Seq.map (fun (a, b) -> hms_interval_exc a b) |> union_seq

let of_sorted_interval_seq ?(skip_invalid : bool = false)
    (s : (int64 * int64) Seq.t) : t =
  let s =
    s
    |> Intervals.Filter.filter_empty
    |> (if skip_invalid then Intervals.Filter.filter_invalid
        else Intervals.Check.check_if_valid)
    |> Seq.filter_map (fun (x, y) ->
        match
          (Date_time'.of_timestamp x, Date_time'.of_timestamp (Int64.pred y))
        with
        | Ok _, Ok _ -> Some (x, y)
        | _, _ -> if skip_invalid then None else raise Interval_is_invalid)
    |> Intervals.Check.check_if_sorted
    |> Intervals.normalize ~skip_filter_invalid:true ~skip_sort:true
  in
  match s () with
  | Seq.Nil -> Empty
  | _ -> Timestamp_interval_seq (default_search_space, s)

let of_sorted_intervals ?(skip_invalid : bool = false)
    (l : (int64 * int64) list) : t =
  l |> CCList.to_seq |> of_sorted_interval_seq ~skip_invalid

let of_intervals ?(skip_invalid : bool = false) (l : (int64 * int64) list) : t =
  let s =
    l
    |> Intervals.Filter.filter_empty_list
    |> (if skip_invalid then Intervals.Filter.filter_invalid_list
        else Intervals.Check.check_if_valid_list)
    |> CCList.filter_map (fun (x, y) ->
        match
          (Date_time'.of_timestamp x, Date_time'.of_timestamp (Int64.pred y))
        with
        | Ok _, Ok _ -> Some (x, y)
        | _, _ -> if skip_invalid then None else raise Interval_is_invalid)
    |> Intervals.Sort.sort_uniq_intervals_list
    |> CCList.to_seq
    |> Intervals.normalize ~skip_filter_invalid:true ~skip_sort:true
  in
  match s () with
  | Seq.Nil -> Empty
  | _ -> Timestamp_interval_seq (default_search_space, s)

let of_interval_seq ?(skip_invalid : bool = false) (s : (int64 * int64) Seq.t) :
  t =
  s |> CCList.of_seq |> of_intervals ~skip_invalid

let of_timestamp_seq ?(skip_invalid = false) timestamps =
  timestamps
  |> Seq.filter_map (fun x ->
      match Date_time'.of_timestamp x with
      | Ok _ -> Some x
      | Error () -> if skip_invalid then None else raise Invalid_timestamp)
  |> Seq.map (fun x -> (x, Int64.succ x))
  |> of_interval_seq

let of_timestamps ?(skip_invalid = false) timestamps =
  timestamps
  |> CCList.filter_map (fun x ->
      match Date_time'.of_timestamp x with
      | Ok _ -> Some x
      | Error () -> if skip_invalid then None else raise Invalid_timestamp)
  |> List.map (fun x -> (x, Int64.succ x))
  |> of_intervals

let full_string_of_weekday (wday : weekday) : string =
  match wday with
  | `Sun -> "Sunday"
  | `Mon -> "Monday"
  | `Tue -> "Tuesday"
  | `Wed -> "Wednesday"
  | `Thu -> "Thursday"
  | `Fri -> "Friday"
  | `Sat -> "Saturday"

let weekday_of_full_string s : (weekday, unit) result =
  match s with
  | "Sunday" -> Ok `Sun
  | "Monday" -> Ok `Mon
  | "Tuesday" -> Ok `Tue
  | "Wednesday" -> Ok `Wed
  | "Thursday" -> Ok `Thu
  | "Friday" -> Ok `Fri
  | "Saturday" -> Ok `Sat
  | _ -> Error ()

let abbr_string_of_weekday (wday : weekday) : string =
  String.sub (full_string_of_weekday wday) 0 3

let weekday_of_abbr_string s : (weekday, unit) result =
  match s with
  | "Sun" -> Ok `Sun
  | "Mon" -> Ok `Mon
  | "Tue" -> Ok `Tue
  | "Wed" -> Ok `Wed
  | "Thu" -> Ok `Thu
  | "Fri" -> Ok `Fri
  | "Sat" -> Ok `Sat
  | _ -> Error ()

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

let month_of_full_string s : (month, unit) result =
  match s with
  | "January" -> Ok `Jan
  | "February" -> Ok `Feb
  | "March" -> Ok `Mar
  | "April" -> Ok `Apr
  | "May" -> Ok `May
  | "June" -> Ok `Jun
  | "July" -> Ok `Jul
  | "August" -> Ok `Aug
  | "September" -> Ok `Sep
  | "October" -> Ok `Oct
  | "November" -> Ok `Nov
  | "December" -> Ok `Dec
  | _ -> Error ()

let abbr_string_of_month (month : month) : string =
  String.sub (full_string_of_month month) 0 3

let month_of_abbr_string s : (month, unit) result =
  match s with
  | "Jan" -> Ok `Jan
  | "Feb" -> Ok `Feb
  | "Mar" -> Ok `Mar
  | "Apr" -> Ok `Apr
  | "May" -> Ok `May
  | "Jun" -> Ok `Jun
  | "Jul" -> Ok `Jul
  | "Aug" -> Ok `Aug
  | "Sep" -> Ok `Sep
  | "Oct" -> Ok `Oct
  | "Nov" -> Ok `Nov
  | "Dec" -> Ok `Dec
  | _ -> Error ()
