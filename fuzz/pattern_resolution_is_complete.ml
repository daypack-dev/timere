open Fuzz_utils
open Span_set_utils

let () =
  Crowbar.add_test ~name:"pattern_resolution_is_complete"
    [ time_zone; search_space; pattern ] (fun tz search_space pattern ->
        Crowbar.guard (search_space <> []);
        let search_start = fst (List.hd search_space) in
        let search_end_exc =
          snd
            (CCOpt.get_exn_or
               "Expected successful retrieval of last element of list"
             @@ Misc_utils.last_element_of_list search_space)
        in
        let search_space_set =
          span_set_of_intervals @@ CCList.to_seq search_space
        in
        let s =
          Resolver.aux_pattern tz search_space pattern |> Resolver.normalize
        in
        match search_space with
        | [] -> Crowbar.check (OSeq.is_empty s)
        | _ ->
          let s' =
            Seq_utils.a_to_b_inc_int64 ~a:search_start.s ~b:search_end_exc.s
            |> OSeq.filter (fun timestamp ->
                Simple_resolver.aux_pattern_mem tz pattern timestamp)
            |> intervals_of_int64s
            |> span_set_of_intervals
            |> Span_set.inter search_space_set
            |> intervals_of_span_set
          in
          let r =
            OSeq.for_all
              (fun (x', y') ->
                 OSeq.exists Timedesc.Span.(fun (x, y) -> x <= x' && y' <= y) s)
              s'
          in
          if not r then
            Crowbar.fail
              (Fmt.str "tz: %s\nsearch_space: %a\npattern: %a\n"
                 (Timedesc.Time_zone.name tz)
                 (Fmt.list (Timedesc.Interval.pp ()))
                 search_space CCSexp.pp
                 (To_sexp.sexp_of_pattern pattern)))
