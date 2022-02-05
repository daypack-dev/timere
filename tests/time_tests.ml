open Test_utils

module Alco = struct
  let union_empty () =
    Alcotest.(check (list (pair span_testable span_testable)))
      "same list" []
      (Time.union [] |> Resolver.resolve |> CCResult.get_exn |> CCList.of_seq)

  let inter_empty () =
    Alcotest.(check (list (pair span_testable span_testable)))
      "same list"
      [ (Constants.timestamp_min, Constants.timestamp_max) ]
      (Time.inter [] |> Resolver.resolve |> CCResult.get_exn |> CCList.of_seq)

  let suite =
    [
      Alcotest.test_case "union_empty" `Quick union_empty;
      Alcotest.test_case "inter_empty" `Quick inter_empty;
    ]
end

module Qc = struct
  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" time (fun t ->
        let t' = t |> To_sexp.to_sexp |> Of_sexp.of_sexp in
        Time.equal t t')

  let union_order_does_not_matter =
    QCheck.Test.make ~count:10 ~name:"union_order_does_not_matter"
      QCheck.(pair (int_bound 10) (time_list 3))
      (fun (rand, l1) ->
         let l2 = permute rand l1 in
         let t1 = Time.union l1 in
         let t2 = Time.union l2 in
         print_endline "=====";
         print_endline (To_sexp.to_sexp_string t1);
         print_endline "^^^^^";
         print_endline (To_sexp.to_sexp_string t2);
         print_endline "=====";
         flush stdout;
         let r1 = OSeq.take 10_000 @@ CCResult.get_exn @@ Resolver.resolve t1 in
         let r2 = OSeq.take 10_000 @@ CCResult.get_exn @@ Resolver.resolve t2 in
         OSeq.equal ~eq:( = ) r1 r2)

  let inter_order_does_not_matter =
    QCheck.Test.make ~count:10 ~name:"inter_order_does_not_matter"
      QCheck.(pair (int_bound 10) (time_list 3))
      (fun (rand, l1) ->
         let l2 = permute rand l1 in
         let t1 = Time.inter l1 in
         let t2 = Time.inter l2 in
         let r1 = OSeq.take 10_000 @@ CCResult.get_exn @@ Resolver.resolve t1 in
         let r2 = OSeq.take 10_000 @@ CCResult.get_exn @@ Resolver.resolve t2 in
         OSeq.equal ~eq:( = ) r1 r2)

  let suite =
    [
      to_of_sexp; (* union_order_does_not_matter;
                   * inter_order_does_not_matter *)
    ]
end
