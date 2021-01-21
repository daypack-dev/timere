open Fuzz_utils

let () =
  Crowbar.add_test ~name:"bounded_intervals_is_sound"
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
             (fun (x, y) ->
                OSeq.mem ~eq:( = ) x s1
                && OSeq.mem ~eq:( = ) y s2
                && Int64.sub y x <= bound
                && not (OSeq.exists (fun x2 -> x < x2 && x2 < y) s2))
             s
           && OSeq.for_all
             (fun (x, y) ->
                let r =
                  OSeq.filter (fun x1 -> x1 < x && Int64.sub x x1 <= bound) s1
                in
                let xr = CCOpt.get_exn @@ Seq_utils.last_element_of_seq r in
                y = Int64.succ x
                && OSeq.mem ~eq:( = ) x s2
                && not (OSeq.exists (fun x2 -> xr < x2 && x2 < x) s2))
             s'))
