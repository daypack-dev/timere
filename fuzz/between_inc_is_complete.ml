open Fuzz_utils

let () =
  Crowbar.add_test ~name:"between_inc_is_complete"
    [ Crowbar.range 100_000; time'; time' ] (fun bound t1 t2 ->
        let bound = Int64.of_int bound in
        let tz = Time_zone.utc in
        let s1 = Resolver.aux tz t1 in
        let s2 = Resolver.aux tz t2 in
        let s =
          Resolver.(
            aux_between Inc tz Resolver.default_search_space bound s1 s2 t1 t2)
        in
        Crowbar.check
          (OSeq.for_all
             (fun (x1, _y1) ->
                match
                  Seq.filter
                    (fun (x2, _y2) -> x1 <= x2 && Int64.sub x2 x1 <= bound)
                    s2 ()
                with
                | Seq.Nil -> true
                | Seq.Cons ((_xr2, yr2), _) -> OSeq.mem ~eq:( = ) (x1, yr2) s)
             s1))
