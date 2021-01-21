open Fuzz_utils

let () =
  Crowbar.add_test ~name:"bounded_intervals_is_complete"
    [ time_zone; Crowbar.range 100_000; points; points ] (fun tz bound p1 p2 ->
        let bound = Int64.of_int bound in
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
        Crowbar.check
          (OSeq.for_all
             (fun x1 ->
                match
                  Seq.filter (fun x2 -> x1 < x2 && Int64.sub x2 x1 <= bound) s2 ()
                with
                | Seq.Nil -> true
                | Seq.Cons (xr2, _) ->
                  OSeq.mem ~eq:( = ) (x1, xr2) s
                  && OSeq.mem ~eq:( = ) (xr2, Int64.succ xr2) s')
             s1))
