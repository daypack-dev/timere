open Fuzz_utils

let () =
  Crowbar.add_test ~name:"between_inc_soundness"
    [ Crowbar.range 100_000; time; time ] (fun bound t1 t2 ->
        let bound = Int64.of_int bound in
        let s1 = CCResult.get_exn @@ Resolver.resolve t1 in
        let s2 = CCResult.get_exn @@ Resolver.resolve t2 in
        let l1 = CCList.of_seq s1 in
        let l2 = CCList.of_seq s2 in
        let s =
          CCResult.get_exn
          @@ Resolver.resolve Time.(between_inc (Duration.of_seconds bound) t1 t2)
        in
        Crowbar.check
          (OSeq.for_all
             (fun (x, y) ->
                match List.filter (fun (x1, _y1) -> x = x1) l1 with
                | [] -> false
                | [ (_xr1, yr1) ] -> (
                    match List.filter (fun (x2, _y2) -> y = x2) l2 with
                    | [] -> false
                    | [ (xr2, _yr2) ] ->
                      not
                        (List.exists (fun (x2, _y2) -> yr1 <= x2 && x2 < xr2) l2)
                    | _ -> false)
                | _ -> false)
             s))
