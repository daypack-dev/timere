open Test_utils

module Alco = struct
  let union_empty () =
    Alcotest.(check (list (pair int64 int64)))
      "same list" []
      (Time.union [] |> Resolver.resolve |> CCResult.get_exn |> CCList.of_seq)

  let inter_empty () =
    Alcotest.(check (list (pair int64 int64)))
      "same list" []
      (Time.inter [] |> Resolver.resolve |> CCResult.get_exn |> CCList.of_seq)

  let after_empty () =
    Alcotest.(check (list (pair int64 int64)))
      "same list" []
      (Time.after (Duration.make ~days:1 ()) Time.empty Time.always
       |> Resolver.resolve
       |> CCResult.get_exn
       |> CCList.of_seq)

  let between_inc_empty () =
    Alcotest.(check (list (pair int64 int64)))
      "same list" []
      (Time.between_inc (Duration.make ~days:1 ()) Time.empty Time.always
       |> Resolver.resolve
       |> CCResult.get_exn
       |> CCList.of_seq)

  let between_exc_empty () =
    Alcotest.(check (list (pair int64 int64)))
      "same list" []
      (Time.between_exc (Duration.make ~days:1 ()) Time.empty Time.always
       |> Resolver.resolve
       |> CCResult.get_exn
       |> CCList.of_seq)

  let suite =
    [
      Alcotest.test_case "union_empty" `Quick union_empty;
      Alcotest.test_case "inter_empty" `Quick inter_empty;
      Alcotest.test_case "after_empty" `Quick after_empty;
      Alcotest.test_case "between_inc_empty" `Quick between_inc_empty;
      Alcotest.test_case "between_exc_empty" `Quick between_exc_empty;
    ]
end

module Qc = struct
  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" time (fun t ->
        let t' = t |> To_sexp.to_sexp |> Of_sexp.of_sexp |> CCResult.get_exn in
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

  let after_empty =
    QCheck.Test.make ~count:10 ~name:"after_empty"
      QCheck.(pair pos_int64 time)
      (fun (bound_offset, t1) ->
         let open QCheck in
         let bound =
           Int64.add Duration.(make ~days:366 () |> to_seconds) bound_offset
         in
         print_endline "=====";
         print_endline (To_sexp.to_sexp_string t1);
         print_endline "=====";
         let s1 =
           CCResult.get_exn
           @@ Resolver.resolve Time.(after (Duration.of_seconds bound) t1 empty)
         in
         let s2 =
           CCResult.get_exn
           @@ Resolver.resolve Time.(after (Duration.of_seconds bound) empty t1)
         in
         OSeq.is_empty s1 && OSeq.is_empty s2)

  let after_soundness =
    QCheck.Test.make ~count:10 ~name:"after_soundness"
      QCheck.(triple pos_int64 time time)
      (fun (bound_offset, t1, t2) ->
         let open QCheck in
         let bound =
           Int64.add Duration.(make ~days:366 () |> to_seconds) bound_offset
         in
         print_endline "=====";
         print_endline (To_sexp.to_sexp_string t1);
         print_endline "^^^^^";
         print_endline (To_sexp.to_sexp_string t2);
         print_endline "=====";
         let s1 = CCResult.get_exn @@ Resolver.resolve t1 in
         let s2 = CCResult.get_exn @@ Resolver.resolve t2 in
         let l1 = CCList.of_seq s1 in
         let s =
           CCResult.get_exn
           @@ Resolver.resolve Time.(after (Duration.of_seconds bound) t1 t2)
         in
         OSeq.for_all
           (fun (x, _y) ->
              match
                List.filter
                  (fun (_x1, y1) -> y1 <= x && Int64.sub x y1 <= bound)
                  l1
              with
              | [] -> false
              | r ->
                let _xr, yr = List.hd @@ List.rev r in
                not (OSeq.exists (fun (x2, _y2) -> yr <= x2 && x2 < x) s2))
           s)

  let after_completeness =
    QCheck.Test.make ~count:10 ~name:"after_completeness"
      QCheck.(triple pos_int64 time time)
      (fun (bound_offset, t1, t2) ->
         let open QCheck in
         let bound =
           Int64.add Duration.(make ~days:366 () |> to_seconds) bound_offset
         in
         let s1 = CCResult.get_exn @@ Resolver.resolve t1 in
         let s2 = CCResult.get_exn @@ Resolver.resolve t2 in
         let l2 = CCList.of_seq s2 in
         let s =
           CCResult.get_exn
           @@ Resolver.resolve Time.(after (Duration.of_seconds bound) t1 t2)
         in
         OSeq.for_all
           (fun (_x1, y1) ->
              match
                List.filter
                  (fun (x2, _y2) -> y1 <= x2 && Int64.sub x2 y1 <= bound)
                  l2
              with
              | [] -> true
              | r :: _ -> OSeq.mem ~eq:( = ) r s)
           s1)

  let suite =
    [
      to_of_sexp;
      union_order_does_not_matter;
      inter_order_does_not_matter;
      (* after_empty;
       * after_soundness;
       * after_completeness; *)
    ]
end
