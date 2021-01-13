open Fuzz_utils

let () =
  Crowbar.add_test ~name:"follow_is_sound" [ Crowbar.range 100_000; time'; time' ]
    (fun bound t1 t2 ->
       let bound = Int64.of_int bound in
       let tz = Time_zone.utc in
       let s1 = Resolver.aux tz t1 in
       let s2 = Resolver.aux tz t2 in
       let s =
         Resolver.aux_follow tz Resolver.default_search_space bound s1 s2 t1 t2
       in
       Crowbar.check
         (OSeq.for_all
            (fun (x, _y) ->
               let r =
                 OSeq.filter
                   (fun (x1, _y1) -> x1 <= x && Int64.sub x x1 <= bound)
                   s1
               in
               match r () with
               | Seq.Nil -> false
               | _ ->
                 let xr, _yr =
                   CCOpt.get_exn @@ Seq_utils.last_element_of_seq r
                 in
                 not (OSeq.exists (fun (x2, _y2) -> xr <= x2 && x2 < x) s2))
            s))
