open Fuzz_utils

let () =
  Crowbar.add_test ~name:"bounded_intervals_is_complete"
    [ time_zone; Crowbar.range 100_000; points; points ] (fun tz bound p1 p2 ->
        let bound = Timedesc.Span.make_small ~s:bound () in
        let s1 = Resolver.aux_points tz Resolver.default_result_space p1 in
        let s2 = Resolver.aux_points tz Resolver.default_result_space p2 in
        let s =
          Resolver.(
            aux_bounded_intervals ~search_space:Resolver.default_result_space tz
              `Whole bound p1 p2)
        in
        let s' =
          Resolver.(
            aux_bounded_intervals ~search_space:Resolver.default_result_space tz
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
                 OSeq.mem ~eq:Time.Interval'.equal (x1, xr2) s
                 && OSeq.mem ~eq:Time.Interval'.equal
                   (xr2, Timedesc.Span.succ xr2)
                   s')
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
