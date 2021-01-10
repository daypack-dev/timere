open Fuzz_utils

let () =
  Crowbar.add_test ~name:"after_soundness" [ Crowbar.range 100_000; time; time ]
    (fun bound t1 t2 ->
       let bound = Int64.of_int bound in
       let tz = Time_zone.utc in
       let s1 = Resolver.aux tz t1 in
       let s2 = Resolver.aux tz t2 in
       let l1 = CCList.of_seq s1 in
       let s =
         Resolver.aux_after tz Time.default_search_space bound s1 s2 t1 t2
       in
       Crowbar.check
         (OSeq.for_all
            (fun (x, _y) ->
               match
                 List.filter
                   (fun (_x1, y1) -> y1 <= x && Int64.sub x y1 <= bound)
                   l1
               with
               | [] -> false
               | r ->
                 let _xr, yr = List.hd @@ List.rev r in
                 not (OSeq.exists (fun (x2, _y2) -> yr <= x2 && x2 < x) s2))
            s))
