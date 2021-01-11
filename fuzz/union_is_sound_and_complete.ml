open Fuzz_utils

let () =
  Crowbar.add_test ~name:"union_is_sound_and_complete" [ time; time ]
    (fun t1 t2 ->
       let tz = Time_zone.utc in
       let s1 = Resolver.aux tz t1 in
       let s2 = Resolver.aux tz t2 in
       let s =
         Resolver.aux_union tz (CCList.to_seq [t1; t2])
       in
       let s' = Time.Intervals.Union.union s1 s2 in
       Crowbar.check (
         OSeq.equal ~eq:( = ) s s'
       )
    )
