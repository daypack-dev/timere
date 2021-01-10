open Fuzz_utils

let () =
  Crowbar.add_test ~name:"between_exc_completeness"
    [ Crowbar.range 100_000; time; time ] (fun bound t1 t2 ->
        let bound = Int64.of_int bound in
        let tz = Time_zone.utc in
        let s1 = Resolver.aux tz t1 in
        let s2 = Resolver.aux tz t2 in
        let l2 = CCList.of_seq s2 in
        let s =
          Resolver.(
            aux_between Exc tz Time.default_search_space bound s1 s2 t1 t2)
        in
        Crowbar.check
          (OSeq.for_all
             (fun (x1, y1) ->
                match
                  List.filter
                    (fun (x2, _y2) -> y1 <= x2 && Int64.sub x2 y1 <= bound)
                    l2
                with
                | [] -> true
                | (_xr, yr) :: _ -> OSeq.mem ~eq:( = ) (x1, yr) s)
             s1))
