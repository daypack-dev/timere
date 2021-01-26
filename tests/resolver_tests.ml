module Qc = struct
  (* let find_follow_empty =
   *   QCheck.Test.make ~count:10_000 ~name:"find_follow_empty"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_follow bound s1 s2 in
   *        (not (OSeq.is_empty s1 || OSeq.is_empty s2)) || OSeq.is_empty s)
   * 
   * let find_follow_soundness =
   *   QCheck.Test.make ~count:10_000 ~name:"find_follow_soundness"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_follow bound s1 s2 in
   *        OSeq.for_all
   *          (fun (x1, _y1) ->
   *             match
   *               List.filter
   *                 (fun (_x2, y2) -> y2 <= x1 && Int64.sub x1 y2 <= bound)
   *                 l1
   *             with
   *             | [] -> false
   *             | r ->
   *               let _xr, yr = List.hd @@ List.rev r in
   *               not (List.exists (fun (x2, _y2) -> yr <= x2 && x2 < x1) l2))
   *          s)
   * 
   * let find_follow_completeness =
   *   QCheck.Test.make ~count:10_000 ~name:"find_follow_completeness"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_follow bound s1 s2 in
   *        List.for_all
   *          (fun (_x1, y1) ->
   *             match
   *               List.filter
   *                 (fun (x2, _y2) -> y1 <= x2 && Int64.sub x2 y1 <= bound)
   *                 l2
   *             with
   *             | [] -> true
   *             | r :: _ -> OSeq.mem ~eq:( = ) r s)
   *          l1) *)

  (* let find_between_inc_empty =
   *   QCheck.Test.make ~count:10_000 ~name:"find_between_inc_empty"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_between_inc bound s1 s2 in
   *        (not (OSeq.is_empty s1 || OSeq.is_empty s2)) || OSeq.is_empty s)
   * 
   * let find_between_inc_soundness =
   *   QCheck.Test.make ~count:10_000 ~name:"find_between_inc_soundness"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_between_inc bound s1 s2 in
   *        OSeq.for_all
   *          (fun (x1, y1) ->
   *             match List.filter (fun (x2, _y2) -> x1 = x2) l1 with
   *             | [] -> false
   *             | [ (_xr1, yr1) ] -> (
   *                 match List.filter (fun (_x2, y2) -> y1 = y2) l2 with
   *                 | [] -> false
   *                 | [ (xr2, _yr2) ] ->
   *                   not
   *                     (List.exists (fun (x2, _y2) -> yr1 <= x2 && x2 < xr2) l2)
   *                 | _ -> false)
   *             | _ -> false)
   *          s)
   * 
   * let find_between_inc_completeness =
   *   QCheck.Test.make ~count:10_000 ~name:"find_between_inc_completeness"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_between_inc bound s1 s2 in
   *        List.for_all
   *          (fun (x1, y1) ->
   *             match
   *               List.filter
   *                 (fun (x2, _y2) -> y1 <= x2 && Int64.sub x2 y1 <= bound)
   *                 l2
   *             with
   *             | [] -> true
   *             | (_xr, yr) :: _ -> OSeq.mem ~eq:( = ) (x1, yr) s)
   *          l1) *)

  (* let find_between_exc_empty =
   *   QCheck.Test.make ~count:10_000 ~name:"find_between_exc_empty"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_between_exc bound s1 s2 in
   *        (not (OSeq.is_empty s1 || OSeq.is_empty s2)) || OSeq.is_empty s)
   * 
   * let find_between_exc_soundness =
   *   QCheck.Test.make ~count:10_000 ~name:"find_between_exc_soundness"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_between_exc bound s1 s2 in
   *        OSeq.for_all
   *          (fun (x1, y1) ->
   *             match List.filter (fun (x2, _y2) -> x1 = x2) l1 with
   *             | [] -> false
   *             | [ (_xr1, yr1) ] -> (
   *                 match List.filter (fun (x2, _y2) -> y1 = x2) l2 with
   *                 | [] -> false
   *                 | [ (xr2, _yr2) ] ->
   *                   not
   *                     (List.exists (fun (x2, _y2) -> yr1 <= x2 && x2 < xr2) l2)
   *                 | _ -> false)
   *             | _ -> false)
   *          s)
   * 
   * let find_between_exc_completeness =
   *   QCheck.Test.make ~count:10_000 ~name:"find_between_exc_completeness"
   *     QCheck.(
   *       triple pos_int64 sorted_time_slots_maybe_gaps
   *         sorted_time_slots_maybe_gaps)
   *     (fun (bound, l1, l2) ->
   *        let s1 = CCList.to_seq l1 in
   *        let s2 = CCList.to_seq l2 in
   *        let s = Resolver.find_between_exc bound s1 s2 in
   *        List.for_all
   *          (fun (x1, y1) ->
   *             match
   *               List.filter
   *                 (fun (x2, _y2) -> y1 <= x2 && Int64.sub x2 y1 <= bound)
   *                 l2
   *             with
   *             | [] -> true
   *             | (xr, _yr) :: _ -> OSeq.mem ~eq:( = ) (x1, xr) s)
   *          l1) *)

  let suite =
    [ (* find_follow_empty;
       * find_follow_soundness;
       * find_follow_completeness;
       * find_between_inc_empty;
       * find_between_inc_soundness;
       * find_between_inc_completeness;
       * find_between_exc_empty;
       * find_between_exc_soundness;
       * find_between_exc_completeness; *) ]
end
