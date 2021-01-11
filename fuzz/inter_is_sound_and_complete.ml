open Fuzz_utils

let () =
  Crowbar.add_test ~name:"inter_is_sound_and_complete" [ Crowbar.list1 time ]
    (fun l ->
       let tz = Time_zone.utc in
       let s =
         Resolver.aux_inter tz (CCList.to_seq l)
         |> Resolver.normalize
       in
       let s' = l |> List.map (Resolver.aux tz) |> CCList.to_seq
                |> Time.Intervals.Inter.inter_multi_seq
       in
       Crowbar.check (
         OSeq.equal ~eq:( = ) s s'
       )
    )
