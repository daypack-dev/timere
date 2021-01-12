open Fuzz_utils

let () =
  Crowbar.add_test ~name:"between_inc_is_sound"
    [ Crowbar.range 100_000; time; time ] (fun bound t1 t2 ->
        let bound = Int64.of_int bound in
        let tz = Time_zone.utc in
        let s1 = Resolver.aux tz t1 in
        let s2 = Resolver.aux tz t2 in
        let s =
          Resolver.(
            aux_between Inc tz Time.default_search_space bound s1 s2 t1 t2)
        in
        Crowbar.check
          (OSeq.for_all
             (fun (x, y) ->
                let r1 = Seq.filter (fun (x1, _y1) -> x = x1) s1 in
                match r1 () with
                | Seq.Nil -> false
                | Seq.Cons ((xr1, _yr1), rest1) -> (
                    match rest1 () with
                    | Seq.Nil -> (
                        let r2 = Seq.filter (fun (_x2, y2) -> y = y2) s2 in
                        match r2 () with
                        | Seq.Nil -> false
                        | Seq.Cons ((xr2, _yr2), rest2) -> (
                            match rest2 () with
                            | Seq.Nil ->
                              not
                                (OSeq.exists
                                   (fun (x2, _y2) -> xr1 <= x2 && x2 < xr2)
                                   r2)
                            | _ -> false))
                    | _ -> false))
             s))
