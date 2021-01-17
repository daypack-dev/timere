open Fuzz_utils

let () =
  Crowbar.add_test ~name:"union_is_sound_and_complete"
    [ time_zone; Crowbar.list time' ] (fun tz l ->
        let s = Resolver.aux_union tz (CCList.to_seq l) |> Resolver.normalize in
        let s' =
          l
          |> List.map (Resolver.aux tz)
          |> CCList.to_seq
          |> Time.Intervals.Union.union_multi_seq
          |> Time.slice_valid_interval
        in
        Crowbar.check (OSeq.equal ~eq:( = ) s s'))
