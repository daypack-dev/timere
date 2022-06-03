open Test_utils

module Int64_set = Set.Make (struct
    type t = int64

    let compare = compare
  end)

module Span_set = Diet.Make (struct
    type t = Timedesc.Span.t

    let compare = Timedesc.Span.compare

    let zero = Timedesc.Span.zero

    let pred = Timedesc.Span.pred

    let succ = Timedesc.Span.succ

    let sub = Timedesc.Span.sub

    let add = Timedesc.Span.add

    let to_string = Timedesc.Span.to_string
  end)

module Alco = struct
  (* let round_robin_simple1 () =
   *   Alcotest.(check (list (pair int64 int64)))
   *     "same list" [ (0L, 1L); (4L, 5L) ]
   *     ([
   *       [ (0L, 1L) ];
   *       [ (-10L, -9L); (-7L, -5L); (-1L, 1L); (0L, 1L); (3L, 5L) ];
   *       [ (4L, 5L) ];
   *     ]
   *       |> List.map CCList.to_seq
   *       |> Time.Intervals.Round_robin.merge_multi_list_round_robin_non_decreasing
   *       |> CCList.of_seq) *)

  let suite =
    [ (* Alcotest.test_case "round_robin_simple1" `Quick round_robin_simple1 *) ]
end

module Qc = struct
  let slice_start =
    QCheck.Test.make ~count:10_000 ~name:"slice_start"
      QCheck.(pair pos_timestamp sorted_time_slots_maybe_gaps)
      (fun (start, l) ->
         l
         |> CCList.to_seq
         |> Time.Intervals.Slice.slice ~start
         |> OSeq.for_all (fun (x, _) -> Timedesc.Span.(start <= x)))

  let slice_end_exc =
    QCheck.Test.make ~count:10_000 ~name:"slice_end_exc"
      QCheck.(pair pos_timestamp sorted_time_slots_maybe_gaps)
      (fun (end_exc, l) ->
         l
         |> CCList.to_seq
         |> Time.Intervals.Slice.slice ~end_exc
         |> OSeq.for_all (fun (_, y) -> Timedesc.Span.(y <= end_exc)))

  let normalize_pairs_are_fine =
    QCheck.Test.make ~count:10_000 ~name:"normalize_pairs_are_fine" time_slots
      (fun l ->
         l
         |> CCList.to_seq
         |> Time.Intervals.normalize
         |> OSeq.for_all (fun (x, y) -> Timedesc.Span.(x <= y)))

  let normalize_time_slots_are_sorted =
    QCheck.Test.make ~count:10_000 ~name:"normalize_time_slots_are_sorted"
      time_slots (fun l ->
          l
          |> CCList.to_seq
          |> Time.Intervals.normalize
          |> OSeq.fold_left
            (fun (res, last) (x, y) ->
               if res then
                 match last with
                 | None -> (true, Some (x, y))
                 | Some (last_start, last_end_exc) ->
                   Timedesc.Span.
                     (last_start <= x && last_end_exc <= x, Some (x, y))
               else (false, None))
            (true, None)
          |> fun (x, _) -> x)

  let normalize_time_slots_are_unique =
    QCheck.Test.make ~count:10_000 ~name:"normalize_time_slots_are_unique"
      time_slots (fun l ->
          let l =
            l |> CCList.to_seq |> Time.Intervals.normalize |> CCList.of_seq
          in
          List.length (List.sort_uniq compare l) = List.length l)

  let normalize_time_slots_are_disjoint_with_gaps =
    QCheck.Test.make ~count:10_000
      ~name:"normalize_time_slots_are_disjoint_with_gaps" time_slots (fun l ->
          l
          |> CCList.to_seq
          |> Time.Intervals.normalize
          |> Seq.fold_left
            (fun (res, last) (x, y) ->
               if res then
                 match last with
                 | None -> (true, Some (x, y))
                 | Some (_, last_end_exc) ->
                   Timedesc.Span.(last_end_exc < x, Some (x, y))
               else (false, None))
            (true, None)
          |> fun (x, _) -> x)

  let normalize_idempotent_wrt_normalized_time_slots =
    QCheck.Test.make ~count:10_000
      ~name:"normalize_idempotent_wrt_normalized_time_slots"
      sorted_time_slots_with_gaps (fun l ->
          l |> CCList.to_seq |> Time.Intervals.normalize |> CCList.of_seq = l)

  let normalize_is_lossless =
    QCheck.Test.make ~count:10_000 ~name:"normalize_is_lossless"
      sorted_time_slots_maybe_gaps (fun l ->
          let original_timestamps =
            List.fold_left
              (fun acc (x, y) -> Span_set.(add (Interval.make x y) acc))
              Span_set.empty l
          in
          let normalized_timestamps =
            l
            |> CCList.to_seq
            |> Time.Intervals.normalize ~skip_sort:true
            |> Seq.fold_left
              (fun acc (x, y) -> Span_set.(add (Interval.make x y) acc))
              Span_set.empty
          in
          Span_set.equal original_timestamps normalized_timestamps)

  let join_time_slots_are_disjoint_with_gaps =
    QCheck.Test.make ~count:10_000
      ~name:"join_time_slots_are_disjoint_with_gaps"
      sorted_time_slots_maybe_gaps (fun l ->
          l
          |> CCList.to_seq
          |> Time.Intervals.join
          |> Seq.fold_left
            (fun (res, last) (x, y) ->
               if res then
                 match last with
                 | None -> (true, Some (x, y))
                 | Some (_, last_end_exc) -> (last_end_exc < x, Some (x, y))
               else (false, None))
            (true, None)
          |> fun (x, _) -> x)

  let join_idempotent_wrt_joined_time_slots =
    QCheck.Test.make ~count:10_000 ~name:"join_idempotent_wrt_joined_time_slots"
      sorted_time_slots_with_gaps (fun l ->
          l |> CCList.to_seq |> Time.Intervals.join |> CCList.of_seq = l)

  let invert_disjoint_from_original =
    QCheck.Test.make ~count:10_000 ~name:"invert_disjoint_from_original"
      QCheck.(triple pos_timestamp pos_timestamp sorted_time_slots_maybe_gaps)
      (fun (start, end_exc, l) ->
         QCheck.assume Timedesc.Span.(start <= end_exc);
         let sliced =
           l
           |> CCList.to_seq
           |> Time.Intervals.Slice.slice ~start ~end_exc
           |> CCList.of_seq
         in
         let inverted =
           l
           |> CCList.to_seq
           |> Time.Intervals.invert ~start ~end_exc
           |> CCList.of_seq
         in
         let sliced_count = List.length sliced in
         let inverted_count = List.length inverted in
         List.length (List.sort_uniq compare (sliced @ inverted))
         = sliced_count + inverted_count)

  let invert_fit_gaps =
    QCheck.Test.make ~count:10_000 ~name:"invert_fit_gaps"
      QCheck.(triple pos_timestamp pos_timestamp sorted_time_slots_maybe_gaps)
      (fun (start, end_exc, l) ->
         QCheck.assume (start < end_exc);
         let res =
           l
           |> CCList.to_seq
           |> Time.Intervals.invert ~start ~end_exc
           |> (fun inverted ->
               OSeq.append
                 (Time.Intervals.Slice.slice ~start ~end_exc (CCList.to_seq l))
                 inverted)
           |> Time.Intervals.normalize
           |> CCList.of_seq
         in
         l <> []
         && List.for_all Timedesc.Span.(fun (x, y) -> y < start || end_exc < x) l
         || [ (start, end_exc) ] = res)

  let relative_complement_result_disjoint_from_not_mem_of =
    QCheck.Test.make ~count:10_000
      ~name:"relative_complement_disjoint_from_not_mem_of"
      QCheck.(pair sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps)
      (fun (mem_of, not_mem_of) ->
         let res =
           Time.Intervals.relative_complement
             ~not_mem_of:(CCList.to_seq not_mem_of) (CCList.to_seq mem_of)
           |> CCList.of_seq
         in
         let not_mem_of_count = List.length not_mem_of in
         let res_count = List.length res in
         List.length (List.sort_uniq compare (not_mem_of @ res))
         = not_mem_of_count + res_count)

  let relative_complement_result_subset_of_mem_of =
    QCheck.Test.make ~count:10_000
      ~name:"relative_complement_result_subset_of_mem_of"
      QCheck.(pair sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps)
      (fun (mem_of, not_mem_of) ->
         let res =
           Time.Intervals.relative_complement
             ~not_mem_of:(CCList.to_seq not_mem_of) (CCList.to_seq mem_of)
         in
         Time.Intervals.Inter.inter (CCList.to_seq mem_of) res
         |> OSeq.equal ~eq:Time.Interval'.equal res)

  let relative_complement_self =
    QCheck.Test.make ~count:10_000 ~name:"relative_complement_self"
      sorted_time_slots_maybe_gaps (fun l ->
          let s = CCList.to_seq l in
          Time.Intervals.relative_complement ~not_mem_of:s s |> CCList.of_seq = [])

  let inter_with_self =
    QCheck.Test.make ~count:10_000 ~name:"inter_with_self"
      sorted_time_slots_maybe_gaps (fun l ->
          let s = l |> CCList.to_seq in
          let res = Time.Intervals.Inter.inter s s in
          OSeq.equal ~eq:Time.Interval'.equal s res)

  let inter_commutative =
    QCheck.Test.make ~count:10_000 ~name:"inter_commutative"
      QCheck.(pair sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps)
      (fun (l1, l2) ->
         let s1 = l1 |> CCList.to_seq in
         let s2 = l2 |> CCList.to_seq in
         let inter1 = Time.Intervals.Inter.inter s1 s2 in
         let inter2 = Time.Intervals.Inter.inter s2 s1 in
         OSeq.equal ~eq:Time.Interval'.equal inter1 inter2)

  let inter_associative =
    QCheck.Test.make ~count:10_000 ~name:"inter_associative"
      QCheck.(
        triple sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps
          sorted_time_slots_maybe_gaps)
      (fun (l1, l2, l3) ->
         let s1 = l1 |> CCList.to_seq in
         let s2 = l2 |> CCList.to_seq in
         let s3 = l3 |> CCList.to_seq in
         let inter1 = Time.Intervals.Inter.(inter (inter s1 s2) s3) in
         let inter2 = Time.Intervals.Inter.(inter s1 (inter s2 s3)) in
         OSeq.equal ~eq:Time.Interval'.equal inter1 inter2)

  let union_with_self =
    QCheck.Test.make ~count:10_000 ~name:"union_with_self"
      sorted_time_slots_with_gaps (fun l ->
          let s = l |> CCList.to_seq in
          let res = Time.Intervals.Union.union s s in
          OSeq.equal ~eq:Time.Interval'.equal s res)

  let union_commutative =
    QCheck.Test.make ~count:10_000 ~name:"union_commutative"
      QCheck.(pair sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps)
      (fun (l1, l2) ->
         let s1 = l1 |> CCList.to_seq in
         let s2 = l2 |> CCList.to_seq in
         let inter1 = Time.Intervals.Union.union s1 s2 in
         let inter2 = Time.Intervals.Union.union s2 s1 in
         OSeq.equal ~eq:Time.Interval'.equal inter1 inter2)

  let union_associative =
    QCheck.Test.make ~count:10_000 ~name:"union_associative"
      QCheck.(
        triple sorted_time_slots_with_gaps sorted_time_slots_with_gaps
          sorted_time_slots_with_gaps)
      (fun (l1, l2, l3) ->
         let s1 = l1 |> CCList.to_seq in
         let s2 = l2 |> CCList.to_seq in
         let s3 = l3 |> CCList.to_seq in
         let res1 = Time.Intervals.(Union.union (Union.union s1 s2) s3) in
         let res2 = Time.Intervals.(Union.union s1 (Union.union s2 s3)) in
         OSeq.equal ~eq:Time.Interval'.equal res1 res2)

  let inter_union_distributive1 =
    QCheck.Test.make ~count:10_000 ~name:"inter_union_distributive1"
      QCheck.(
        triple sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps
          sorted_time_slots_maybe_gaps)
      (fun (l1, l2, l3) ->
         let s1 = l1 |> CCList.to_seq in
         let s2 = l2 |> CCList.to_seq in
         let s3 = l3 |> CCList.to_seq in
         let res1 = Time.Intervals.(Union.union s1 (Inter.inter s2 s3)) in
         let res2 =
           Time.Intervals.(Inter.inter (Union.union s1 s2) (Union.union s1 s3))
         in
         OSeq.equal ~eq:Time.Interval'.equal res1 res2)

  let inter_union_distributive2 =
    QCheck.Test.make ~count:10_000 ~name:"inter_union_distributive2"
      QCheck.(
        triple sorted_time_slots_with_gaps sorted_time_slots_maybe_gaps
          sorted_time_slots_maybe_gaps)
      (fun (l1, l2, l3) ->
         let s1 = l1 |> CCList.to_seq in
         let s2 = l2 |> CCList.to_seq in
         let s3 = l3 |> CCList.to_seq in
         let res1 = Time.Intervals.(Inter.inter s1 (Union.union s2 s3)) in
         let res2 =
           Time.Intervals.(Union.union (Inter.inter s1 s2) (Inter.inter s1 s3))
         in
         OSeq.equal ~eq:Time.Interval'.equal res1 res2)

  let suite =
    [
      slice_start;
      slice_end_exc;
      normalize_pairs_are_fine;
      normalize_time_slots_are_sorted;
      normalize_time_slots_are_unique;
      normalize_time_slots_are_disjoint_with_gaps;
      normalize_idempotent_wrt_normalized_time_slots;
      normalize_is_lossless;
      join_time_slots_are_disjoint_with_gaps;
      join_idempotent_wrt_joined_time_slots;
      invert_disjoint_from_original;
      invert_fit_gaps;
      relative_complement_result_disjoint_from_not_mem_of;
      relative_complement_result_subset_of_mem_of;
      relative_complement_self;
      inter_with_self;
      inter_commutative;
      inter_associative;
      union_with_self;
      union_commutative;
      union_associative;
      inter_union_distributive1;
      inter_union_distributive2;
    ]
end
