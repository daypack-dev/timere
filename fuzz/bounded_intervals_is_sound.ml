open Fuzz_utils

let () =
  Crowbar.add_test ~name:"bounded_intervals_is_sound"
    [ time_zone; Crowbar.range 100_000; points; points ] (fun tz bound p1 p2 ->
        let bound = Span.make_small ~s:bound () in
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
            (fun (x, y) ->
               OSeq.mem ~eq:( = ) x s1
               && OSeq.mem ~eq:( = ) y s2
               && Span.(y - x <= bound)
               && not (OSeq.exists Span.(fun x2 -> x < x2 && x2 < y) s2))
            s
          && OSeq.for_all
            (fun (x, y) ->
               let r =
                 OSeq.filter Span.(fun x1 -> x1 < x && x - x1 <= bound) s1
               in
               let xr = CCOpt.get_exn @@ Seq_utils.last_element_of_seq r in
               y = Span.succ x
               && OSeq.mem ~eq:Span.equal x s2
               && not (OSeq.exists Span.(fun x2 -> xr < x2 && x2 < x) s2))
            s'
        in
        if not r then
          Crowbar.fail
            (Fmt.str "tz: %s, bound: %a\np1: %a, p2: %a\n" (Time_zone.name tz)
               Printers.pp_span bound CCSexp.pp
               (To_sexp.sexp_of_points p1)
               CCSexp.pp
               (To_sexp.sexp_of_points p2)))
