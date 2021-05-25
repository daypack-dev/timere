open Fuzz_utils

let () =
  Crowbar.add_test ~name:"bounded_intervals_is_sound"
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
            (fun (x, y) ->
               OSeq.mem ~eq:( = ) x s1
               && OSeq.mem ~eq:( = ) y s2
               && Timedesc.Span.(y - x <= bound)
               && not (OSeq.exists Timedesc.Span.(fun x2 -> x < x2 && x2 < y) s2))
            s
          && OSeq.for_all
            (fun (x, y) ->
               let r =
                 OSeq.filter
                   Timedesc.Span.(fun x1 -> x1 < x && x - x1 <= bound)
                   s1
               in
               let xr =
                 CCOpt.get_exn_or
                   "Expected successful retrieval of last element of seq"
                 @@ Seq_utils.last_element_of_seq r
               in
               y = Timedesc.Span.succ x
               && OSeq.mem ~eq:Timedesc.Span.equal x s2
               && not
                 (OSeq.exists Timedesc.Span.(fun x2 -> xr < x2 && x2 < x) s2))
            s'
        in
        if not r then
          Crowbar.fail
            (Fmt.str "tz: %s, bound: %a\np1: %a, p2: %a\n"
               (Timedesc.Time_zone.name tz)
               Timedesc.Span.pp bound CCSexp.pp
               (To_sexp.sexp_of_points p1)
               CCSexp.pp
               (To_sexp.sexp_of_points p2)))
