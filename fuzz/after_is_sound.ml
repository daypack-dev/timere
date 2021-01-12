open Fuzz_utils

let () =
  Crowbar.add_test ~name:"after_is_sound" [ Crowbar.range 100_000; time; time ]
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
                   (fun (x1, _y1) -> x1 <= x && Int64.sub x x1 <= bound)
                   l1
               with
               | [] -> false
               | r ->
                 let xr, _yr = CCOpt.get_exn @@ Misc_utils.last_element_of_list r in
                 not (OSeq.exists (fun (x2, _y2) -> xr <= x2 && x2 < x) s2))
            s))
