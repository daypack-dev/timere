open Fuzz_utils

let () =
  Crowbar.add_test ~name:"after_empty" [ Crowbar.range 100_000; time'; time' ]
    (fun bound t1 t2 ->
       let bound = Int64.of_int bound in
       let tz = Time_zone.utc in
       let s1 = Resolver.aux tz t1 in
       let s2 = Resolver.aux tz t2 in
       let s =
         Resolver.aux_after tz Resolver.default_search_space bound s1 s2 t1 t2
       in
       Crowbar.check
         ((not (OSeq.is_empty s1 && OSeq.is_empty s2)) || OSeq.is_empty s))
