open Fuzz_utils

let () =
  Crowbar.add_test ~name:"bounded_intervals_empty"
    [ time_zone; Crowbar.range 100_000; points; points ] (fun tz bound p1 p2 ->
        let bound = Timedesc.Span.make_small ~s:bound () in
        let s1 = Resolver.aux_points tz Resolver.default_search_space p1 in
        let s2 = Resolver.aux_points tz Resolver.default_search_space p2 in
        let s =
          Resolver.(
            aux_bounded_intervals tz Resolver.default_search_space `Whole bound p1
              p2)
        in
        let s' =
          Resolver.(
            aux_bounded_intervals tz Resolver.default_search_space `Snd bound p1
              p2)
        in
        let r =
          (not (OSeq.is_empty s1 && OSeq.is_empty s2))
          || (OSeq.is_empty s && OSeq.is_empty s')
        in
        if not r then
          Crowbar.fail
            (Fmt.str "tz: %s, bound: %a\np1: %a, p2: %a\n"
               (Timedesc.Time_zone.name tz)
               Timedesc.Span.pp bound CCSexp.pp
               (To_sexp.sexp_of_points p1)
               CCSexp.pp
               (To_sexp.sexp_of_points p2)))
