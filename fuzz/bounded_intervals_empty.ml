open Fuzz_utils

let () =
  Crowbar.add_test ~name:"pattern_intervals_empty"
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
        (not (OSeq.is_empty s1 && OSeq.is_empty s2))
        || List.for_all OSeq.is_empty [ r1; r2; r3; r4 ]
      in
      if not r then
        Crowbar.fail
          (Fmt.str "tz: %s, bound: %a\np1: %a, p2: %a\n"
             (Timedesc.Time_zone.name tz)
             Timedesc.Span.pp bound CCSexp.pp
             (To_sexp.sexp_of_points p1)
             CCSexp.pp
             (To_sexp.sexp_of_points p2)))
