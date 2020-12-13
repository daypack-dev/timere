type tz_offset_s = int

type timestamp = int64

let tz_offset_s_utc = 0

exception Interval_is_invalid

exception Interval_is_empty

exception Intervals_are_not_sorted

exception Intervals_are_not_disjoint

exception Month_day_ranges_are_invalid

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

let next_weekday (wday : weekday) : weekday =
  match wday with
  | `Sun -> `Mon
  | `Mon -> `Tue
  | `Tue -> `Wed
  | `Wed -> `Thu
  | `Thu -> `Fri
  | `Fri -> `Sat
  | `Sat -> `Sun

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

let month_lt m1 m2 = tm_int_of_month m1 < tm_int_of_month m2

let month_le m1 m2 = tm_int_of_month m1 <= tm_int_of_month m2

let month_gt m1 m2 = tm_int_of_month m1 > tm_int_of_month m2

let month_ge m1 m2 = tm_int_of_month m1 >= tm_int_of_month m2

let compare_weekday (d1 : weekday) (d2 : weekday) : int =
  compare (tm_int_of_weekday d1) (tm_int_of_weekday d2)

let weekday_lt d1 d2 = tm_int_of_weekday d1 < tm_int_of_weekday d2

let weekday_le d1 d2 = tm_int_of_weekday d1 <= tm_int_of_weekday d2

let weekday_gt d1 d2 = tm_int_of_weekday d1 > tm_int_of_weekday d2

let weekday_ge d1 d2 = tm_int_of_weekday d1 >= tm_int_of_weekday d2

let zero_tm_sec tm = Unix.{ tm with tm_sec = 0 }

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

  let to_string ((start, end_exc) : t) : string =
    Printf.sprintf "[%Ld, %Ld)" start end_exc

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
          else l |> List.to_seq |> Check.check_if_valid |> List.of_seq)
      |> List.sort Interval.compare

    let sort_uniq_intervals_list ?(skip_check = false)
        (intervals : Interval.t list) : Interval.t list =
      intervals
      |> (fun l ->
          if skip_check then l
          else l |> List.to_seq |> Check.check_if_valid |> List.of_seq)
      |> List.sort_uniq Interval.compare

    let sort_uniq_intervals ?(skip_check = false) (intervals : Interval.t Seq.t)
      : Interval.t Seq.t =
      intervals
      |> (fun s -> if skip_check then s else Check.check_if_valid s)
      |> List.of_seq
      |> List.sort_uniq Interval.compare
      |> List.to_seq

    let sort_intervals ?(skip_check = false) (intervals : Interval.t Seq.t) :
      Interval.t Seq.t =
      intervals
      |> (fun s -> if skip_check then s else Check.check_if_valid s)
      |> List.of_seq
      |> List.sort Interval.compare
      |> List.to_seq
  end

  module Join_internal = struct
    let join (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      let rec aux cur intervals =
        match intervals () with
        | Seq.Nil -> (
            match cur with None -> Seq.empty | Some x -> Seq.return x )
        | Seq.Cons ((start, end_exc), rest) -> (
            match cur with
            | None -> aux (Some (start, end_exc)) rest
            | Some cur -> (
                match Interval.join cur (start, end_exc) with
                | Some x -> aux (Some x) rest
                | None ->
                  (* cannot be merged, add time slot being carried to the sequence *)
                  fun () -> Seq.Cons (cur, aux (Some (start, end_exc)) rest) )
          )
      in
      aux None intervals
  end

  let join ?(skip_check = false) intervals =
    intervals
    |> (fun s ->
        if skip_check then s
        else s |> Check.check_if_valid |> Check.check_if_sorted)
    |> Join_internal.join

  module Normalize = struct
    let normalize ?(skip_filter_invalid = false) ?(skip_filter_empty = false)
        ?(skip_sort = false) intervals =
      intervals
      |> (fun s -> if skip_filter_invalid then s else Filter.filter_invalid s)
      |> (fun s -> if skip_filter_empty then s else Filter.filter_empty s)
      |> (fun s -> if skip_sort then s else Sort.sort_uniq_intervals s)
      |> Join_internal.join

    let normalize_list_in_seq_out ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false) intervals =
      intervals
      |> List.to_seq
      |> normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort
  end

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

  module Slice_rev_internal = struct
    let slice_start ~start (intervals : Interval.t Seq.t) : Interval.t Seq.t =
      let rec aux acc start intervals =
        match intervals () with
        | Seq.Nil -> List.rev acc |> List.to_seq
        | Seq.Cons ((ts_start, ts_end_exc), slots) ->
          if start <= ts_start then
            (* entire time slot is after start, add to acc *)
            aux ((ts_start, ts_end_exc) :: acc) start slots
          else if ts_start < start && start < ts_end_exc then
            (* time slot spans across the start mark, split time slot *)
            aux ((start, ts_end_exc) :: acc) start slots
          else
            (* time slot is before start mark, do nothing *)
            aux acc start Seq.empty
      in
      aux [] start intervals

    let slice_end_exc ~end_exc (intervals : Interval.t Seq.t) : Interval.t Seq.t
      =
      let rec aux end_exc intervals =
        match intervals () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons ((ts_start, ts_end_exc), slots) ->
          if ts_end_exc <= end_exc then
            (* entire time slot is before end_exc mark, do nothing *)
            intervals
          else if ts_start < end_exc && end_exc < ts_end_exc then
            (* time slot spans across the end_exc mark, split time slot *)
            OSeq.cons (ts_start, end_exc) slots
          else
            (* time slot is after end_exc mark, move to next time slot *)
            aux end_exc slots
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

    let slice_rev ?(skip_check = false) ?start ?end_exc intervals =
      intervals
      |> (fun s ->
          if skip_check then s
          else s |> Check.check_if_valid |> Check.check_if_sorted_rev)
      |> (fun s ->
          match start with
          | None -> s
          | Some start -> Slice_rev_internal.slice_start ~start s)
      |> fun s ->
      match end_exc with
      | None -> s
      | Some end_exc -> Slice_rev_internal.slice_end_exc ~end_exc s
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

    let inter_multi_list ?(skip_check = false)
        (interval_batches : Interval.t Seq.t list) : Interval.t Seq.t =
      List.to_seq interval_batches |> inter_multi_seq ~skip_check
  end

  module Union = struct
    let union ?(skip_check = false) (intervals1 : Interval.t Seq.t)
        (intervals2 : Interval.t Seq.t) : Interval.t Seq.t =
      let rec aux intervals1 intervals2 =
        match (intervals1 (), intervals2 ()) with
        | Seq.Nil, s | s, Seq.Nil -> fun () -> s
        | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
          let ts1 () = Seq.Cons (x1, rest1) in
          let ts2 () = Seq.Cons (x2, rest2) in
          if Interval.le x1 x2 then fun () -> Seq.Cons (x1, aux rest1 ts2)
          else fun () -> Seq.Cons (x2, aux rest2 ts1)
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
      |> Normalize.normalize ~skip_filter_invalid:true ~skip_sort:true

    let union_multi_seq ?(skip_check = false)
        (interval_batches : Interval.t Seq.t Seq.t) : Interval.t Seq.t =
      Seq.fold_left
        (fun acc intervals -> union ~skip_check acc intervals)
        Seq.empty interval_batches

    let union_multi_list ?(skip_check = false)
        (interval_batches : Interval.t Seq.t list) : Interval.t Seq.t =
      List.to_seq interval_batches |> union_multi_seq ~skip_check
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
      |> Seq.flat_map (fun l -> List.to_seq l |> Seq.filter_map (fun x -> x))
      |> Normalize.normalize ~skip_filter_invalid:true ~skip_sort:true

    let merge_multi_seq_round_robin_non_decreasing ?(skip_check = false)
        (batches : Interval.t Seq.t Seq.t) : Interval.t Seq.t =
      batches
      |> List.of_seq
      |> merge_multi_list_round_robin_non_decreasing ~skip_check
  end

  let chunk ?(skip_check = false) ?(drop_partial = false) ~chunk_size
      (intervals : Interval.t Seq.t) : Interval.t Seq.t =
    let rec aux intervals =
      match intervals () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((start, end_exc), rest) ->
        let chunk_end_exc = min end_exc (start +^ chunk_size) in
        let size = chunk_end_exc -^ start in
        if size = 0L || (size < chunk_size && drop_partial) then aux rest
        else
          let rest () = Seq.Cons ((chunk_end_exc, end_exc), rest) in
          fun () -> Seq.Cons ((start, chunk_end_exc), aux rest)
    in
    intervals
    |> (fun s -> if skip_check then s else s |> Check.check_if_valid)
    |> aux

  module Sum = struct
    let sum_length ?(skip_check = false) (intervals : Interval.t Seq.t) : int64
      =
      intervals
      |> (fun s -> if skip_check then s else Check.check_if_valid s)
      |> Seq.fold_left
        (fun acc (start, end_exc) -> acc +^ (end_exc -^ start))
        0L

    let sum_length_list ?(skip_check = false) (intervals : Interval.t list) :
      int64 =
      intervals |> List.to_seq |> sum_length ~skip_check
  end

  module Bound = struct
    let min_start_and_max_end_exc ?(skip_check = false)
        (intervals : Interval.t Seq.t) : (int64 * int64) option =
      intervals
      |> (fun s -> if skip_check then s else Check.check_if_valid s)
      |> Seq.fold_left
        (fun acc (start, end_exc) ->
           match acc with
           | None -> Some (start, end_exc)
           | Some (min_start, max_end_exc) ->
             Some (min min_start start, max max_end_exc end_exc))
        None

    let min_start_and_max_end_exc_list ?(skip_check = false)
        (intervals : Interval.t list) : (int64 * int64) option =
      intervals |> List.to_seq |> min_start_and_max_end_exc ~skip_check
  end

  let shift_list ~offset (intervals : Interval.t list) : Interval.t list =
    List.map
      (fun (start, end_exc) -> (start +^ offset, end_exc +^ offset))
      intervals

  let equal (intervals1 : Interval.t list) (intervals2 : Interval.t list) : bool
    =
    let intervals1 =
      intervals1 |> List.to_seq |> Normalize.normalize |> List.of_seq
    in
    let intervals2 =
      intervals2 |> List.to_seq |> Normalize.normalize |> List.of_seq
    in
    intervals1 = intervals2

  let a_is_subset_of_b ~(a : Interval.t Seq.t) ~(b : Interval.t Seq.t) : bool =
    let inter = Inter.inter a b |> List.of_seq in
    let a = List.of_seq a in
    a = inter
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
    |> Option.map (fun (x, y) -> `Range_exc (of_int64 x, of_int64 y))

  let is_valid (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
      (t : a range) : bool =
    match modulo with
    | None -> (
        match int64_range_of_range ~to_int64 t with
        | `Range_inc (x, y) -> x <= y
        | `Range_exc (x, y) -> x <= y )
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
                |> Seq.map of_int64 )
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
                |> Seq.map of_int64 )

    let flatten_into_list (type a) ~(modulo : int64 option)
        ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (t : a range) : a list
      =
      flatten_into_seq ~modulo ~to_int64 ~of_int64 t |> List.of_seq
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
        Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t |> List.of_seq
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
        | _, _ -> None )
    | `Range_exc (x, y) -> (
        match (x, y) with
        | Ok x, Ok y -> Some (`Range_exc (x, y))
        | _, _ -> None )
end

module Range_small = struct
  let int_range_of_range (type a) ~(to_int : a -> int) (x : a Range.range) :
    int Range.range =
    let f (x, y) = (to_int x, to_int y) in
    Range.map ~f_inc:f ~f_exc:f x

  let int_exc_range_of_range (type a) ~(to_int : a -> int) (x : a Range.range) :
    int * int =
    match x with
    | `Range_inc (x, y) -> (to_int x, y |> to_int |> Int.succ)
    | `Range_exc (x, y) -> (to_int x, to_int y)

  let inc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a)
      (x : a Range.range) : a * a =
    match x with
    | `Range_inc (x, y) -> (x, y)
    | `Range_exc (x, y) -> (x, y |> to_int |> Int.pred |> of_int)

  let exc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a)
      (x : a Range.range) : a * a =
    match x with
    | `Range_inc (x, y) -> (x, y |> to_int |> Int.succ |> of_int)
    | `Range_exc (x, y) -> (x, y)

  let join (type a) ~(to_int : a -> int) ~(of_int : int -> a)
      (x : a Range.range) (y : a Range.range) : a Range.range option =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    Range.join ~to_int64 ~of_int64 x y

  module Flatten = struct
    let flatten_into_seq (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (t : a Range.range) : a Seq.t =
      let modulo = Option.map Int64.of_int modulo in
      let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
      let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
      Range.Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t

    let flatten_into_list (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (t : a Range.range) : a list =
      flatten_into_seq ~modulo ~to_int ~of_int t |> List.of_seq
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
        Flatten.flatten_into_seq ~modulo ~to_int ~of_int t |> List.of_seq
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
      |> Intervals.Normalize.normalize ~skip_filter_invalid ~skip_filter_empty
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
      l |> List.to_seq |> flatten ~modulo ~to_int64 ~of_int64 |> List.of_seq
  end

  module Of_seq = struct
    let range_seq_of_seq (type a) ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false)
        ~(modulo : int64 option) ~(to_int64 : a -> int64)
        ~(of_int64 : int64 -> a) (s : a Seq.t) : a Range.range Seq.t =
      s
      |> Seq.map (fun x -> `Range_inc (x, x))
      |> normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
        ~to_int64 ~of_int64

    let range_list_of_seq (type a) ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false)
        ~(modulo : int64 option) ~(to_int64 : a -> int64)
        ~(of_int64 : int64 -> a) (s : a Seq.t) : a Range.range list =
      range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty ~skip_sort
        ~modulo ~to_int64 ~of_int64 s
      |> List.of_seq
  end

  module Of_list = struct
    let range_seq_of_list (type a) ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false)
        ~(modulo : int64 option) ~(to_int64 : a -> int64)
        ~(of_int64 : int64 -> a) (l : a list) : a Range.range Seq.t =
      List.to_seq l
      |> Of_seq.range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty
        ~skip_sort ~modulo ~to_int64 ~of_int64

    let range_list_of_list (type a) ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false)
        ~(modulo : int64 option) ~(to_int64 : a -> int64)
        ~(of_int64 : int64 -> a) (l : a list) : a Range.range list =
      List.to_seq l
      |> Of_seq.range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty
        ~skip_sort ~modulo ~to_int64 ~of_int64
      |> List.of_seq
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
      val range_seq_of_seq : t Seq.t -> t Range.range Seq.t

      val range_list_of_seq : t Seq.t -> t Range.range list
    end

    module Of_list : sig
      val range_seq_of_list : t list -> t Range.range Seq.t

      val range_list_of_list : t list -> t Range.range list
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
      let range_seq_of_seq (s : t Seq.t) : t Range.range Seq.t =
        Of_seq.range_seq_of_seq ~modulo ~to_int64 ~of_int64 s

      let range_list_of_seq (s : t Seq.t) : t Range.range list =
        Of_seq.range_list_of_seq ~modulo ~to_int64 ~of_int64 s
    end

    module Of_list = struct
      let range_seq_of_list (l : t list) : t Range.range Seq.t =
        List.to_seq l |> Of_seq.range_seq_of_seq

      let range_list_of_list (l : t list) : t Range.range list =
        List.to_seq l |> Of_seq.range_seq_of_seq |> List.of_seq
    end
  end
end

module Ranges_small = struct
  let normalize (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
      ~(to_int : a -> int) ~(of_int : int -> a) (s : a Range.range Seq.t) :
    a Range.range Seq.t =
    let modulo = Option.map Int64.of_int modulo in
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    Ranges.normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
      ~to_int64 ~of_int64 s

  module Check = struct
    let seq_is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
        (s : a Range.range Seq.t) : bool =
      let modulo = Option.map Int64.of_int modulo in
      let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
      Ranges.Check.seq_is_valid ~modulo ~to_int64 s

    let list_is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
        (l : a Range.range list) : bool =
      let modulo = Option.map Int64.of_int modulo in
      let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
      Ranges.Check.list_is_valid ~modulo ~to_int64 l
  end

  module Flatten = struct
    let flatten (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (s : a Range.range Seq.t) : a Seq.t =
      let modulo = Option.map Int64.of_int modulo in
      let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
      let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
      Ranges.Flatten.flatten ~modulo ~to_int64 ~of_int64 s

    let flatten_list (type a) ~(modulo : int option) ~(to_int : a -> int)
        ~(of_int : int -> a) (l : a Range.range list) : a list =
      l |> List.to_seq |> flatten ~modulo ~to_int ~of_int |> List.of_seq
  end

  module Of_seq = struct
    let range_seq_of_seq (type a) ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
        ~(to_int : a -> int) ~(of_int : int -> a) (s : a Seq.t) :
      a Range.range Seq.t =
      s
      |> Seq.map (fun x -> `Range_inc (x, x))
      |> normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
        ~to_int ~of_int

    let range_list_of_seq (type a) ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
        ~(to_int : a -> int) ~(of_int : int -> a) (s : a Seq.t) :
      a Range.range list =
      range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty ~skip_sort
        ~modulo ~to_int ~of_int s
      |> List.of_seq
  end

  module Of_list = struct
    let range_seq_of_list (type a) ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
        ~(to_int : a -> int) ~(of_int : int -> a) (l : a list) :
      a Range.range Seq.t =
      List.to_seq l
      |> Of_seq.range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty
        ~skip_sort ~modulo ~to_int ~of_int

    let range_list_of_list (type a) ?(skip_filter_invalid = false)
        ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
        ~(to_int : a -> int) ~(of_int : int -> a) (l : a list) :
      a Range.range list =
      List.to_seq l
      |> Of_seq.range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty
        ~skip_sort ~modulo ~to_int ~of_int
      |> List.of_seq
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
      let range_seq_of_seq (s : t Seq.t) : t Range.range Seq.t =
        Of_seq.range_seq_of_seq ~modulo ~to_int ~of_int s

      let range_list_of_seq (s : t Seq.t) : t Range.range list =
        Of_seq.range_list_of_seq ~modulo ~to_int ~of_int s
    end

    module Of_list = struct
      let range_seq_of_list (l : t list) : t Range.range Seq.t =
        List.to_seq l |> Of_seq.range_seq_of_seq

      let range_list_of_list (l : t list) : t Range.range list =
        List.to_seq l |> Of_seq.range_seq_of_seq |> List.of_seq
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

let second_of_day_of_hms x =
  Duration.make ~hours:x.hour ~minutes:x.minute ~seconds:x.second ()
  |> Result.get_ok
  |> Duration.to_seconds
  |> Int64.to_int

let hms_of_second_of_day x =
  let ({ hours; minutes; seconds; _ } : Duration.t) =
    x |> Int64.of_int |> Duration.of_seconds |> Result.get_ok
  in
  Result.get_ok @@ make_hms ~hour:hours ~minute:minutes ~second:seconds

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

    let of_int x = x |> weekday_of_tm_int |> Result.get_ok
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

    let of_int x = x |> month_of_human_int |> Result.get_ok
  end)

module Year_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

let cur_timestamp () : int64 = Unix.time () |> Int64.of_float

module Date_time = struct
  exception Invalid_date_time

  type t = {
    year : int;
    month : month;
    day : int;
    hour : int;
    minute : int;
    second : int;
    tz_offset_s : int;
  }

  let to_ptime_date_time (x : t) : Ptime.date * Ptime.time =
    ( (x.year, human_int_of_month x.month, x.day),
      ((x.hour, x.minute, x.second), x.tz_offset_s) )

  let of_ptime_date_time
      (((year, month, day), ((hour, minute, second), tz_offset_s)) :
         Ptime.date * Ptime.time) : (t, unit) result =
    match month_of_human_int month with
    | Ok month -> Ok { year; month; day; hour; minute; second; tz_offset_s }
    | Error () -> Error ()

  let to_timestamp (x : t) : int64 =
    Ptime.of_date_time (to_ptime_date_time x)
    |> Option.get
    |> Ptime.to_float_s
    |> Int64.of_float

  let of_timestamp ?(tz_offset_s_of_date_time = 0) (x : int64) :
    (t, unit) result =
    match Ptime.of_float_s (Int64.to_float x) with
    | None -> Error ()
    | Some x ->
      x
      |> Ptime.to_date_time ~tz_offset_s:tz_offset_s_of_date_time
      |> of_ptime_date_time

  let make ~year ~month ~day ~hour ~minute ~second ~tz_offset_s =
    let dt = { year; month; day; hour; minute; second; tz_offset_s } in
    match Ptime.of_date_time (to_ptime_date_time dt) with
    | None -> Error ()
    | Some _ -> Ok dt

  let make_exn ~year ~month ~day ~hour ~minute ~second ~tz_offset_s =
    match make ~year ~month ~day ~hour ~minute ~second ~tz_offset_s with
    | Error () -> raise Invalid_date_time
    | Ok x -> x

  let min =
    Ptime.min |> Ptime.to_date_time |> of_ptime_date_time |> Result.get_ok

  let max =
    Ptime.max |> Ptime.to_date_time |> of_ptime_date_time |> Result.get_ok

  let cur ?(tz_offset_s_of_date_time = 0) () : (t, unit) result =
    cur_timestamp () |> of_timestamp ~tz_offset_s_of_date_time

  let compare (x : t) (y : t) : int =
    match compare x.year y.year with
    | 0 -> (
        match
          compare (human_int_of_month x.month) (human_int_of_month y.month)
        with
        | 0 -> (
            match compare x.day y.day with
            | 0 -> (
                match compare x.hour y.hour with
                | 0 -> (
                    match compare x.minute y.minute with
                    | 0 -> compare x.second y.second
                    | n -> n )
                | n -> n )
            | n -> n )
        | n -> n )
    | n -> n

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

module Date_time_set = Set.Make (struct
    type t = Date_time.t

    let compare = Date_time.compare
  end)

module Check = struct
  let timestamp_is_valid (x : int64) : bool =
    match Date_time.of_timestamp x with Ok _ -> true | Error () -> false

  let second_is_valid ~(second : int) : bool = 0 <= second && second < 60

  let minute_second_is_valid ~(minute : int) ~(second : int) : bool =
    0 <= minute && minute < 60 && second_is_valid ~second

  let hour_minute_second_is_valid ~(hour : int) ~(minute : int) ~(second : int)
    : bool =
    (0 <= hour && hour < 24) && minute_second_is_valid ~minute ~second
end

let next_hour_minute ~(hour : int) ~(minute : int) : (int * int, unit) result =
  if Check.hour_minute_second_is_valid ~hour ~minute ~second:0 then
    if minute < 59 then Ok (hour, succ minute) else Ok (succ hour mod 24, 0)
  else Error ()

module Pattern = struct
  type pattern = {
    years : int list;
    months : month list;
    month_days : int list;
    weekdays : weekday list;
    hours : int list;
    minutes : int list;
    seconds : int list;
    timestamps : int64 list;
  }

  let equal p1 p2 = p1 = p2

  type error =
    | Invalid_years of int list
    | Invalid_month_days of int list
    | Invalid_hours of int list
    | Invalid_minutes of int list
    | Invalid_seconds of int list
    | Invalid_timestamps of int64 list

  type range_pattern = pattern Range.range

  module Check = struct
    let check_pattern (x : pattern) : (unit, error) result =
      let invalid_years = List.filter (fun x -> x < 0 || 9999 < x) x.years in
      let invalid_month_days =
        List.filter (fun x -> x < 1 || 31 < x) x.month_days
      in
      let invalid_hours = List.filter (fun x -> x < 0 || 23 < x) x.hours in
      let invalid_minutes = List.filter (fun x -> x < 0 || 59 < x) x.minutes in
      let invalid_seconds = List.filter (fun x -> x < 0 || 59 < x) x.seconds in
      let invalid_timestamps =
        List.filter
          (fun x -> Result.is_error (Date_time.of_timestamp x))
          x.timestamps
      in
      match invalid_years with
      | [] -> (
          match invalid_month_days with
          | [] -> (
              match invalid_hours with
              | [] -> (
                  match invalid_minutes with
                  | [] -> (
                      match invalid_seconds with
                      | [] -> (
                          match invalid_timestamps with
                          | [] -> Ok ()
                          | l -> Error (Invalid_timestamps l) )
                      | l -> Error (Invalid_seconds l) )
                  | l -> Error (Invalid_minutes l) )
              | l -> Error (Invalid_hours l) )
          | l -> Error (Invalid_month_days l) )
      | l -> Error (Invalid_years l)

    let check_range_pattern (x : range_pattern) : (unit, error) result =
      match x with
      | `Range_inc (x, y) | `Range_exc (x, y) -> (
          match check_pattern x with
          | Error e -> Error e
          | Ok () -> (
              match check_pattern y with Error e -> Error e | Ok () -> Ok () ) )
  end

  let union p1 p2 =
    let compare_month m1 m2 =
      compare (tm_int_of_month m1) (tm_int_of_month m2)
    in
    let compare_weekday d1 d2 =
      compare (tm_int_of_weekday d1) (tm_int_of_weekday d2)
    in
    let aux compare l1 l2 = List.sort_uniq compare (List.merge compare l1 l2) in
    let union_ints (l1 : int list) (l2 : int list) = aux compare l1 l2 in
    let union_int64s (l1 : int64 list) (l2 : int64 list) = aux compare l1 l2 in
    let union_months l1 l2 = aux compare_month l1 l2 in
    let union_weekdays l1 l2 = aux compare_weekday l1 l2 in
    {
      years = union_ints p1.years p2.years;
      months = union_months p1.months p2.months;
      month_days = union_ints p1.month_days p2.month_days;
      weekdays = union_weekdays p1.weekdays p2.weekdays;
      hours = union_ints p1.hours p2.hours;
      minutes = union_ints p1.minutes p2.minutes;
      seconds = union_ints p1.seconds p2.seconds;
      timestamps = union_int64s p1.timestamps p2.timestamps;
    }

  let inter p1 p2 =
    let aux l1 l2 = List.filter (fun x -> List.mem x l2) l1 in
    {
      years = aux p1.years p2.years;
      months = aux p1.months p2.months;
      month_days = aux p1.month_days p2.month_days;
      weekdays = aux p1.weekdays p2.weekdays;
      hours = aux p1.hours p2.hours;
      minutes = aux p1.minutes p2.minutes;
      seconds = aux p1.seconds p2.seconds;
      timestamps = aux p1.timestamps p2.timestamps;
    }
end

type sign_expr =
  | Pos
  | Neg

type unary_op =
  | Not
  | Every
  | Skip_n_points of int
  | Skip_n_intervals of int
  | Next_n_points of int
  | Next_n_intervals of int
  | Chunk of {
      chunk_size : int64;
      drop_partial : bool;
    }
  | Shift of int64
  | Lengthen of int64
  | Change_tz_offset_s of int

type branching_days =
  | Month_days of int Range.range list
  | Weekdays of weekday Range.range list

type branching = {
  years : int Range.range list;
  months : month Range.range list;
  days : branching_days;
  hmss : hms Range.range list;
}

let branching_equal b1 b2 = b1 = b2

type search_space = Interval.t list

let default_search_space_start = Date_time.(to_timestamp min)

let default_search_space_end_exc = Date_time.(to_timestamp max)

let default_search_space : search_space =
  [ (default_search_space_start, default_search_space_end_exc) ]

type t =
  | Timestamp_interval_seq of search_space * (int64 * int64) Seq.t
  | Pattern of search_space * Pattern.pattern
  | Branching of search_space * branching
  | Unary_op of search_space * unary_op * t
  | Interval_inc of search_space * timestamp * timestamp
  | Interval_exc of search_space * timestamp * timestamp
  | Round_robin_pick_list of search_space * t list
  | Inter_list of search_space * t list
  | Union_list of search_space * t list
  | After of search_space * t * t
  | Between_inc of search_space * t * t
  | Between_exc of search_space * t * t

let equal t1 t2 =
  let rec aux t1 t2 =
    match (t1, t2) with
    | Timestamp_interval_seq (_, s1), Timestamp_interval_seq (_, s2) ->
      OSeq.equal ~eq:( = ) s1 s2
    | Pattern (_, p1), Pattern (_, p2) -> Pattern.equal p1 p2
    | Branching (_, b1), Branching (_, b2) -> branching_equal b1 b2
    | Unary_op (_, op1, t1), Unary_op (_, op2, t2) -> op1 = op2 && aux t1 t2
    | Interval_inc (_, x11, x12), Interval_inc (_, x21, x22)
    | Interval_exc (_, x11, x12), Interval_exc (_, x21, x22) ->
      x11 = x21 && x12 = x22
    | After (_, x11, x12), After (_, x21, x22)
    | Between_inc (_, x11, x12), Between_inc (_, x21, x22)
    | Between_exc (_, x11, x12), Between_exc (_, x21, x22) ->
      aux x11 x21 && aux x12 x22
    | Round_robin_pick_list (_, l1), Round_robin_pick_list (_, l2)
    | Inter_list (_, l1), Inter_list (_, l2)
    | Union_list (_, l1), Union_list (_, l2) ->
      List.for_all2 aux l1 l2
    | _ -> false
  in
  aux t1 t2

let chunk ?(drop_partial = false) (chunk_size : Duration.t) (t : t) : t =
  Unary_op
    ( default_search_space,
      Chunk { chunk_size = Duration.to_seconds chunk_size; drop_partial },
      t )

let shift (offset : Duration.t) (t : t) : t =
  Unary_op (default_search_space, Shift (Duration.to_seconds offset), t)

let lengthen (x : Duration.t) (t : t) : t =
  Unary_op (default_search_space, Lengthen (Duration.to_seconds x), t)

let inter (l : t list) : t =
  let flatten s =
    Seq.flat_map
      (fun x ->
         match x with Inter_list (_, l) -> List.to_seq l | _ -> Seq.return x)
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
               | None -> Some pat
               | Some acc -> Some (Pattern.inter acc pat) )
           | _ -> acc)
        None patterns
    in
    match pattern with
    | None -> rest
    | Some pat -> OSeq.cons (Pattern (default_search_space, pat)) rest
  in
  let l = l |> List.to_seq |> flatten |> inter_patterns |> List.of_seq in
  Inter_list (default_search_space, l)

let union (l : t list) : t =
  let flatten s =
    Seq.flat_map
      (fun x ->
         match x with Union_list (_, l) -> List.to_seq l | _ -> Seq.return x)
      s
  in
  let union_patterns s =
    let patterns, rest =
      OSeq.partition (fun x -> match x with Pattern _ -> true | _ -> false) s
    in
    let pattern =
      Seq.fold_left
        (fun acc x ->
           match x with
           | Pattern (_, pat) -> (
               match acc with
               | None -> Some pat
               | Some acc -> Some (Pattern.union acc pat) )
           | _ -> acc)
        None patterns
    in
    match pattern with
    | None -> rest
    | Some pat -> OSeq.cons (Pattern (default_search_space, pat)) rest
  in
  let l = l |> List.to_seq |> flatten |> union_patterns |> List.of_seq in
  Union_list (default_search_space, l)

let round_robin_pick (l : t list) : t =
  Round_robin_pick_list (default_search_space, l)

let first_point (a : t) : t = Unary_op (default_search_space, Next_n_points 1, a)

let take_n_points (n : int) (t : t) : t =
  if n < 0 then invalid_arg "take_n_points: n < 0"
  else Unary_op (default_search_space, Next_n_points n, t)

let skip_n_points (n : int) (t : t) : t =
  if n < 0 then invalid_arg "skip_n_points: n < 0"
  else Unary_op (default_search_space, Skip_n_points n, t)

let first (t : t) : t = Unary_op (default_search_space, Next_n_intervals 1, t)

let take_n (n : int) (t : t) : t =
  if n < 0 then invalid_arg "take_n: n < 0"
  else Unary_op (default_search_space, Next_n_intervals n, t)

let skip_n (n : int) (t : t) : t =
  if n < 0 then invalid_arg "skip_n: n < 0"
  else Unary_op (default_search_space, Skip_n_intervals n, t)

let interval_inc (a : timestamp) (b : timestamp) : t =
  match Date_time.of_timestamp a with
  | Error () -> invalid_arg "interval_inc: invalid timestamp"
  | Ok _ -> (
      match Date_time.of_timestamp b with
      | Error () -> invalid_arg "interval_inc: invalid timestamp"
      | Ok _ ->
        if a <= b then Interval_inc (default_search_space, a, b)
        else invalid_arg "interval_inc: a > b" )

let interval_exc (a : timestamp) (b : timestamp) : t =
  match Date_time.of_timestamp a with
  | Error () -> invalid_arg "interval_exc: invalid timestamp"
  | Ok _ -> (
      match Date_time.of_timestamp b with
      | Error () -> invalid_arg "interval_exc: invalid timestamp"
      | Ok _ ->
        if a <= b then Interval_exc (default_search_space, a, b)
        else invalid_arg "interval_exc: a > b" )

let interval_dt_inc (a : Date_time.t) (b : Date_time.t) : t =
  let a = Date_time.to_timestamp a in
  let b = Date_time.to_timestamp b in
  if a <= b then Interval_inc (default_search_space, a, b)
  else invalid_arg "interval_dt_inc: a > b"

let interval_dt_exc (a : Date_time.t) (b : Date_time.t) : t =
  let a = Date_time.to_timestamp a in
  let b = Date_time.to_timestamp b in
  if a <= b then Interval_exc (default_search_space, a, b)
  else invalid_arg "interval_dt_exc: a > b"

let not (a : t) : t = Unary_op (default_search_space, Not, a)

let change_tz_offset_s offset t =
  Unary_op (default_search_space, Change_tz_offset_s offset, t)

let safe_month_day_range_inc ~years ~months =
  let contains_non_leap_year =
    years
    |> List.to_seq
    |> Year_ranges.Flatten.flatten
    |> OSeq.exists (fun year -> Stdlib.not (is_leap_year ~year))
  in
  let leap_year = 2020 in
  let non_leap_year = 2019 in
  let rec aux start end_inc months =
    match months () with
    | Seq.Nil -> (start, end_inc)
    | Seq.Cons (month, rest) ->
      let count =
        match month with
        | `Feb ->
          if contains_non_leap_year then
            day_count_of_month ~year:non_leap_year ~month
          else day_count_of_month ~year:leap_year ~month
        | _ -> day_count_of_month ~year:leap_year ~month
      in
      aux (max (-count) start) (min count end_inc) rest
  in
  aux (-31) 31 (Month_ranges.Flatten.flatten @@ List.to_seq @@ months)

let pattern ?(strict = false) ?(years = []) ?(months = []) ?(month_days = [])
    ?(weekdays = []) ?(hours = []) ?(minutes = []) ?(seconds = [])
    ?(timestamps = []) () : t =
  if
    List.for_all
      (fun year -> Date_time.min.year <= year && year <= Date_time.max.year)
      years
    && List.for_all (fun x -> -31 <= x && x <= 31 && x <> 0) month_days
    && List.for_all (fun x -> 0 <= x && x < 24) hours
    && List.for_all (fun x -> 0 <= x && x < 60) minutes
    && List.for_all (fun x -> 0 <= x && x < 60) seconds
    && List.for_all (fun x -> x >= 0L) timestamps
  then
    let years = List.sort_uniq compare years in
    let months = List.sort_uniq compare_month months in
    let is_okay =
      Stdlib.not strict
      ||
      let safe_month_day_start, safe_month_day_end_inc =
        let years =
          match years with
          | [] -> [ `Range_inc (Date_time.min.year, Date_time.max.year) ]
          | _ -> Year_ranges.Of_list.range_list_of_list years
        in
        let months =
          match months with
          | [] -> [ `Range_inc (`Jan, `Dec) ]
          | _ -> Month_ranges.Of_list.range_list_of_list months
        in
        safe_month_day_range_inc ~years ~months
      in
      List.for_all
        (fun x -> safe_month_day_start <= x && x <= safe_month_day_end_inc)
        month_days
    in
    if is_okay then
      Pattern
        ( default_search_space,
          {
            Pattern.years;
            months;
            month_days = List.sort_uniq compare month_days;
            weekdays = List.sort_uniq compare_weekday weekdays;
            hours = List.sort_uniq compare hours;
            minutes = List.sort_uniq compare minutes;
            seconds = List.sort_uniq compare seconds;
            timestamps = List.sort_uniq compare timestamps;
          } )
    else invalid_arg "pattern"
  else invalid_arg "pattern"

let month_day_ranges_are_valid_strict ~safe_month_day_range_inc day_ranges =
  let safe_month_day_start, safe_month_day_end_inc = safe_month_day_range_inc in
  day_ranges
  |> List.to_seq
  |> Month_day_ranges.Flatten.flatten
  |> Seq.filter (fun mday -> mday <> 0)
  |> OSeq.for_all (fun mday ->
      safe_month_day_start <= mday && mday <= safe_month_day_end_inc)

let month_day_ranges_are_valid_relaxed day_range =
  month_day_ranges_are_valid_strict ~safe_month_day_range_inc:(-31, 31)
    day_range

let branching ?(allow_out_of_range_month_day = false) ?(years = [])
    ?(months = []) ?(days = Month_days []) ?(hmss = []) () : t =
  let years =
    match years with
    | [] -> [ `Range_inc (Date_time.min.year, Date_time.max.year) ]
    | _ -> years
  in
  let months =
    match months with [] -> [ `Range_inc (`Jan, `Dec) ] | _ -> months
  in
  let days =
    match days with
    | Month_days [] | Weekdays [] -> Ok (Month_days [ `Range_inc (1, 31) ])
    | Month_days days ->
      if
        if allow_out_of_range_month_day then
          month_day_ranges_are_valid_relaxed days
        else
          month_day_ranges_are_valid_strict
            ~safe_month_day_range_inc:
              (safe_month_day_range_inc ~years ~months)
            days
      then Ok (Month_days days)
      else Error ()
    | Weekdays _ -> Ok days
  in
  let hmss =
    match hmss with
    | [] ->
      [
        `Range_inc
          ( { hour = 0; minute = 0; second = 0 },
            { hour = 23; minute = 59; second = 59 } );
      ]
    | _ -> hmss
  in
  let p_year year = Date_time.min.year <= year && year <= Date_time.max.year in
  let p_hms hms =
    0 <= hms.hour
    && hms.hour <= 23
    && 0 <= hms.minute
    && hms.minute < 60
    && 0 <= hms.second
    && hms.second < 60
  in
  match days with
  | Error () -> raise Month_day_ranges_are_invalid
  | Ok days ->
    if
      List.for_all
        (fun year_range ->
           match year_range with
           | `Range_inc (y1, y2) | `Range_exc (y1, y2) ->
             p_year y1 && p_year y2)
        years
      && Year_ranges.Check.list_is_valid years
      && Month_ranges.Check.list_is_valid months
      && ( match days with
          | Month_days _ -> true
          | Weekdays days -> Weekday_ranges.Check.list_is_valid days )
      && List.for_all
        (fun hms_range ->
           match hms_range with
           | `Range_inc (hms1, hms2) | `Range_exc (hms1, hms2) ->
             p_hms hms1 && p_hms hms2)
        hmss
      && Hms_ranges.Check.list_is_valid hmss
    then
      let years =
        years
        |> List.to_seq
        |> Year_ranges.normalize ~skip_filter_invalid:true
        |> List.of_seq
      in
      let months =
        months
        |> List.to_seq
        |> Month_ranges.normalize ~skip_filter_invalid:true
        |> List.of_seq
      in
      let hmss =
        hmss
        |> List.to_seq
        |> Hms_ranges.normalize ~skip_filter_invalid:true
        |> List.of_seq
      in
      Branching (default_search_space, { years; months; days; hmss })
    else invalid_arg "branching"

let years years = pattern ~years ()

let months months = pattern ~months ()

let month_days month_days = pattern ~month_days ()

let weekdays weekdays = pattern ~weekdays ()

let hours hours = pattern ~hours ()

let minutes minutes = pattern ~minutes ()

let seconds seconds = pattern ~seconds ()

let timestamps timestamps = pattern ~timestamps ()

let always = pattern ()

let after (t1 : t) (t2 : t) : t = After (default_search_space, t1, t2)

let between_inc (t1 : t) (t2 : t) : t =
  Between_inc (default_search_space, t1, t2)

let between_exc (t1 : t) (t2 : t) : t =
  Between_exc (default_search_space, t1, t2)

let date_time (date_time : Date_time.t) : t =
  date_time
  |> Date_time.to_timestamp
  |> fun x ->
  Timestamp_interval_seq (default_search_space, Seq.return (x, Int64.succ x))

let of_sorted_intervals_seq ?(skip_invalid : bool = false)
    (s : (int64 * int64) Seq.t) : t =
  Timestamp_interval_seq
    ( default_search_space,
      s
      |> Intervals.Filter.filter_empty
      |> ( if skip_invalid then Intervals.Filter.filter_invalid
           else Intervals.Check.check_if_valid )
      |> Intervals.Check.check_if_sorted
      |> Intervals.Normalize.normalize ~skip_filter_invalid:true ~skip_sort:true
    )

let of_sorted_intervals ?(skip_invalid : bool = false)
    (l : (int64 * int64) list) : t =
  l |> List.to_seq |> of_sorted_intervals_seq ~skip_invalid

let of_intervals ?(skip_invalid : bool = false) (l : (int64 * int64) list) : t =
  Timestamp_interval_seq
    ( default_search_space,
      l
      |> Intervals.Filter.filter_empty_list
      |> ( if skip_invalid then Intervals.Filter.filter_invalid_list
           else Intervals.Check.check_if_valid_list )
      |> Intervals.Sort.sort_uniq_intervals_list
      |> List.to_seq
      |> Intervals.Normalize.normalize ~skip_filter_invalid:true ~skip_sort:true
    )

let of_intervals_seq ?(skip_invalid : bool = false) (s : (int64 * int64) Seq.t)
  : t =
  s |> List.of_seq |> of_intervals ~skip_invalid

let empty = of_intervals []

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
