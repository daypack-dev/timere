open Fuzz_utils

let () =
  Crowbar.add_test ~name:"union_is_sound_and_complete"
    [ time_zone; Crowbar.list time ] (fun tz l' ->
        let l = List.map Resolver.t_of_ast l' in
        let s = Resolver.aux_union tz (CCList.to_seq l) |> Resolver.normalize in
        let s' =
          l
          |> List.map (Resolver.aux tz)
          |> CCList.to_seq
          |> Time.Intervals.Union.union_multi_seq
          |> Time.slice_valid_interval
        in
        let r = OSeq.equal ~eq:Time.Interval'.equal s s' in
        if not r then
          Crowbar.fail
            (Fmt.str "tz: %s, times: %a\n"
               (Timedesc.Time_zone.name tz)
               (Fmt.list Printers.pp_sexp)
               l'))
