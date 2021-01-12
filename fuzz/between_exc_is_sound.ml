open Fuzz_utils

let () =
  Crowbar.add_test ~name:"between_exc_is_sound"
    [ Crowbar.range 100_000; time; time ] (fun bound t1 t2 ->
        let bound = Int64.of_int bound in
        let tz = Time_zone.utc in
        let s1 = Resolver.aux tz t1 in
        let s2 = Resolver.aux tz t2 in
        let s =
          Resolver.(
            aux_between Exc tz Time.default_search_space bound s1 s2 t1 t2)
        in
        Crowbar.check
          (OSeq.for_all
             (fun (x, y) ->
                let r1 =
                  Seq.filter (fun (x1, _y1) -> x = x1) s1
                in
                match r1 () with
                | Seq.Nil -> false
                | Seq.Cons ((_xr1, yr1), rest1) -> (
                    match rest1 () with
                    | Seq.Nil -> (
                        let r2 =
                          Seq.filter (fun (x2, _y2) -> y = x2) s2
                        in
                        match r2 () with
                        | Seq.Nil -> false
                        | Seq.Cons ((xr2, _yr2), rest2) -> (
                            match rest2 () with
                            | Seq.Nil ->
                              not
                                (OSeq.exists (fun (x2, _y2) -> yr1 <= x2 && x2 < xr2) s2)
                            | _ -> false
                          )
                      )
                    | _ -> false)
             )
             s))
