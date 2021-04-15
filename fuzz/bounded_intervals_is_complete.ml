open Fuzz_utils

let () =
  Crowbar.add_test ~name:"bounded_intervals_is_complete"
    [ time_zone; Crowbar.range 100_000; points; points ] (fun tz bound p1 p2 ->
        let bound = Span.make ~s:(Int64.of_int bound) () in
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
          OSeq.for_all
            (fun x1 ->
               match
                 Seq.filter Span.(fun x2 -> x1 < x2 && x2 - x1 <= bound) s2 ()
               with
               | Seq.Nil -> true
               | Seq.Cons (xr2, _) ->
                 OSeq.mem ~eq:Time.Interval'.equal (x1, xr2) s
                 && OSeq.mem ~eq:Time.Interval'.equal (xr2, Span.succ xr2) s')
            s1
        in
        if not r then
          Crowbar.fail
            (Fmt.str "tz: %s, bound: %a\np1: %a, p2: %a\n" (Time_zone.name tz)
               Printers.pp_span bound CCSexp.pp
               (To_sexp.sexp_of_points p1)
               CCSexp.pp
               (To_sexp.sexp_of_points p2)))
