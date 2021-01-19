open Fuzz_utils

let () =
  Crowbar.add_test ~name:"inter_order_does_not_matter"
    [ Crowbar.int; Crowbar.list time ] (fun rand l1 ->
        let l2 = permute rand l1 in
        let t1 = Time.inter l1 in
        let t2 = Time.inter l2 in
        let r1 = CCResult.get_exn @@ Resolver.resolve t1 in
        let r2 = CCResult.get_exn @@ Resolver.resolve t2 in
        Crowbar.check (OSeq.equal ~eq:( = ) r1 r2))
