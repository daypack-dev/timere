open Fuzz_utils

let () =
  Crowbar.add_test ~name:"pattern_intervals_is_complete"
    [ time_zone; Crowbar.range 100_000; points; points ] (fun tz bound p1 p2 ->
      let bound = Timedesc.Span.make_small ~s:bound () in
      let s1 = Resolver.aux_points tz Resolver.default_result_space p1 in
      let s2 = Resolver.aux_points tz Resolver.default_result_space p2 in
      let r1 =
        Resolver.(
          aux_pattern_intervals ~search_space:Resolver.default_result_space tz
            `Whole_exc bound p1 p2)
      in
      let r2 =
        Resolver.(
          aux_pattern_intervals ~search_space:Resolver.default_result_space tz
            `Whole_inc bound p1 p2)
      in
      let r3 =
        Resolver.(
          aux_pattern_intervals ~search_space:Resolver.default_result_space tz
            `Fst bound p1 p2)
      in
      let r4 =
        Resolver.(
          aux_pattern_intervals ~search_space:Resolver.default_result_space tz
            `Snd bound p1 p2)
      in
      let r =
        OSeq.for_all
          (fun x1 ->
            match
              Seq.filter
                Timedesc.Span.(fun x2 -> x1 < x2 && x2 - x1 <= bound)
                s2 ()
            with
            | Seq.Nil -> true
            | Seq.Cons (xr2, _) ->
                OSeq.mem ~eq:Time.Interval'.equal (x1, xr2) r1
                && OSeq.mem ~eq:Time.Interval'.equal
                     (x1, Timedesc.Span.succ xr2)
                     r2
                && OSeq.mem ~eq:Time.Interval'.equal
                     (x1, Timedesc.Span.succ x1)
                     r3
                && OSeq.mem ~eq:Time.Interval'.equal
                     (xr2, Timedesc.Span.succ xr2)
                     r4)
          s1
      in
      if not r then
        Crowbar.fail
          (Fmt.str "tz: %s, bound: %a\np1: %a, p2: %a\n"
             (Timedesc.Time_zone.name tz)
             Timedesc.Span.pp bound CCSexp.pp
             (To_sexp.sexp_of_points p1)
             CCSexp.pp
             (To_sexp.sexp_of_points p2)))
