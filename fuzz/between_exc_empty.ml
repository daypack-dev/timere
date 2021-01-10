open Fuzz_utils

let () =
  Crowbar.add_test ~name:"between_exc_empty"
    [ Crowbar.range 100_000; time; time ] (fun bound t1 t2 ->
        let bound = Int64.of_int bound in
        let s1 = CCResult.get_exn @@ Resolver.resolve t1 in
        let s2 = CCResult.get_exn @@ Resolver.resolve t2 in
        let s =
          CCResult.get_exn
          @@ Resolver.resolve Time.(between_exc (Duration.of_seconds bound) t1 t2)
        in
        Crowbar.check
          ((not (OSeq.is_empty s1 && OSeq.is_empty s2)) || OSeq.is_empty s))
