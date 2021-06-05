open Test_utils

module Alco = struct
  let floor_case0 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:0L ())
      Timedesc.Span.(floor @@ make ~s:0L ~ns:1_000 ())

  let floor_case1 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:0L ())
      Timedesc.Span.(floor @@ make ~s:0L ~ns:999_999_999 ())

  let floor_case2 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:10L ())
      Timedesc.Span.(floor @@ make ~s:10L ())

  let floor_case3 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-1L) ())
      Timedesc.Span.(floor @@ make ~s:(-1L) ~ns:1_000 ())

  let floor_case4 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-1L) ())
      Timedesc.Span.(floor @@ make ~s:(-1L) ~ns:999_999_999 ())

  let floor_case5 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-10L) ())
      Timedesc.Span.(floor @@ make ~s:(-10L) ())

  let ceil_case0 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:1L ())
      Timedesc.Span.(ceil @@ make ~s:0L ~ns:1 ())

  let ceil_case1 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:1L ())
      Timedesc.Span.(ceil @@ make ~s:0L ~ns:999_999_999 ())

  let ceil_case2 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:10L ())
      Timedesc.Span.(ceil @@ make ~s:10L ())

  let ceil_case3 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-1L) ())
      Timedesc.Span.(ceil @@ make ~s:(-2L) ~ns:1 ())

  let ceil_case4 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-1L) ())
      Timedesc.Span.(ceil @@ make ~s:(-2L) ~ns:999_999_999 ())

  let ceil_case5 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-10L) ())
      Timedesc.Span.(ceil @@ make ~s:(-10L) ())

  let round_case0 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:0L ())
      Timedesc.Span.(round @@ make ~s:0L ~ns:1_000 ())

  let round_case1 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:1L ())
      Timedesc.Span.(round @@ make ~s:0L ~ns:999_999_999 ())

  let round_case2 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:0L ())
      Timedesc.Span.(round @@ make ~s:0L ~ns:499_999_999 ())

  let round_case3 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:1L ())
      Timedesc.Span.(round @@ make ~s:0L ~ns:500_000_000 ())

  let round_case4 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:10L ())
      Timedesc.Span.(round @@ make ~s:10L ())

  let round_case5 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-2L) ())
      Timedesc.Span.(round @@ make ~s:(-2L) ~ns:1_000 ())

  let round_case6 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-1L) ())
      Timedesc.Span.(round @@ make ~s:(-2L) ~ns:999_999_999 ())

  let round_case7 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-2L) ())
      Timedesc.Span.(round @@ make ~s:(-2L) ~ns:499_999_999 ())

  let round_case8 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-1L) ())
      Timedesc.Span.(round @@ make ~s:(-2L) ~ns:500_000_000 ())

  let round_case9 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Span.make ~s:(-10L) ())
      Timedesc.Span.(round @@ make ~s:(-10L) ())

  let edge_case0 () =
    Alcotest.(check bool)
      "same span"
      (let s, ns =
         Timedesc.Span.(to_s_ns @@ make ~s:(-10L) ~ns:CCInt.min_int ())
       in
       0 <= ns
       && ns < 1_000_000_000
       && Int64.add
            (Int64.mul (-10L) 1_000_000_000L)
            (Int64.of_int CCInt.min_int)
          = Int64.add (Int64.mul s 1_000_000_000L) (Int64.of_int ns))
      true

  let edge_case1 () =
    Alcotest.(check bool)
      "same span"
      (let s, ns =
         Timedesc.Span.(to_s_ns @@ make ~s:10L ~ns:CCInt.min_int ())
       in
       0 <= ns
       && ns < 1_000_000_000
       && Int64.add (Int64.mul 10L 1_000_000_000L) (Int64.of_int CCInt.min_int)
          = Int64.add (Int64.mul s 1_000_000_000L) (Int64.of_int ns))
      true

  let edge_case2 () =
    Alcotest.(check bool)
      "same span"
      (let s, ns =
         Timedesc.Span.(to_s_ns @@ make ~s:10L ~ns:CCInt.max_int ())
       in
       0 <= ns
       && ns < 1_000_000_000
       && Int64.add (Int64.mul 10L 1_000_000_000L) (Int64.of_int CCInt.max_int)
          = Int64.add (Int64.mul s 1_000_000_000L) (Int64.of_int ns))
      true

  let edge_case3 () =
    Alcotest.(check bool)
      "same span"
      (let s, ns =
         Timedesc.Span.(to_s_ns @@ make ~s:(-10L) ~ns:CCInt.max_int ())
       in
       0 <= ns
       && ns < 1_000_000_000
       && Int64.add
            (Int64.mul (-10L) 1_000_000_000L)
            (Int64.of_int CCInt.max_int)
          = Int64.add (Int64.mul s 1_000_000_000L) (Int64.of_int ns))
      true

  let suite =
    [
      Alcotest.test_case "floor_case0" `Quick floor_case0;
      Alcotest.test_case "floor_case1" `Quick floor_case1;
      Alcotest.test_case "floor_case2" `Quick floor_case2;
      Alcotest.test_case "floor_case3" `Quick floor_case3;
      Alcotest.test_case "floor_case4" `Quick floor_case4;
      Alcotest.test_case "floor_case5" `Quick floor_case5;
      Alcotest.test_case "ceil_case0" `Quick ceil_case0;
      Alcotest.test_case "ceil_case1" `Quick ceil_case1;
      Alcotest.test_case "ceil_case2" `Quick ceil_case2;
      Alcotest.test_case "ceil_case3" `Quick ceil_case3;
      Alcotest.test_case "ceil_case4" `Quick ceil_case4;
      Alcotest.test_case "ceil_case5" `Quick ceil_case5;
      Alcotest.test_case "round_case0" `Quick round_case0;
      Alcotest.test_case "round_case1" `Quick round_case1;
      Alcotest.test_case "round_case2" `Quick round_case2;
      Alcotest.test_case "round_case3" `Quick round_case3;
      Alcotest.test_case "round_case4" `Quick round_case4;
      Alcotest.test_case "round_case5" `Quick round_case5;
      Alcotest.test_case "round_case6" `Quick round_case6;
      Alcotest.test_case "round_case7" `Quick round_case7;
      Alcotest.test_case "round_case8" `Quick round_case8;
      Alcotest.test_case "round_case9" `Quick round_case9;
      Alcotest.test_case "edge_case0" `Quick edge_case0;
      Alcotest.test_case "edge_case1" `Quick edge_case1;
      Alcotest.test_case "edge_case2" `Quick edge_case2;
      Alcotest.test_case "edge_case3" `Quick edge_case3;
    ]
end

let normalize (x : Timedesc.Span.t) : Timedesc.Span.t =
  let s, ns = Timedesc.Span.to_s_ns x in
  Timedesc.Span.make ~s ~ns ()

module Qc = struct
  let make_is_lossless =
    QCheck.Test.make ~count:100_000 ~name:"make_is_lossless"
      QCheck.(pair int64 int)
      (fun (s, ns) ->
        let span = Timedesc.Span.make ~s ~ns () in
        Int64.add (Int64.mul s 1_000_000_000L) (Int64.of_int ns)
        = Int64.add
            (Int64.mul (Timedesc.Span.get_s span) 1_000_000_000L)
            (Int64.of_int @@ Timedesc.Span.get_ns_offset span))

  let make_result_ns_is_within_bound =
    QCheck.Test.make ~count:1_000_000 ~name:"make_result_ns_is_within_bound"
      QCheck.(pair int64 int)
      (fun (s, ns) ->
        let span = Timedesc.Span.make ~s ~ns () in
        let ns = Timedesc.Span.get_ns_offset span in
        0 <= ns && ns < 1_000_000_000)

  let normalize_is_idempotent =
    QCheck.Test.make ~count:100_000 ~name:"normalize_is_idempotent" timestamp
      (fun x -> Timedesc.Span.(equal (normalize x) (normalize @@ normalize x)))

  let add_sub =
    QCheck.Test.make ~count:100_000 ~name:"add_sub"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Timedesc.Span.(equal x (x + y - y)))

  let sub_add =
    QCheck.Test.make ~count:100_000 ~name:"sub_add"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Timedesc.Span.(equal x (x - y + y)))

  let sub_self =
    QCheck.Test.make ~count:100_000 ~name:"sub_self" timestamp (fun x ->
        Timedesc.Span.(equal zero (x - x)))

  let double_neg =
    QCheck.Test.make ~count:100_000 ~name:"double_neg" timestamp (fun x ->
        Timedesc.Span.(equal x (neg @@ neg x)))

  let neg =
    QCheck.Test.make ~count:100_000 ~name:"neg" timestamp (fun x ->
        Timedesc.Span.(equal x (neg (neg x - x) - x)))

  let abs =
    QCheck.Test.make ~count:100_000 ~name:"abs" timestamp (fun x ->
        Timedesc.Span.(equal (abs x) (abs (neg x))))

  let abs_is_non_neg =
    QCheck.Test.make ~count:100_000 ~name:"abs_is_non_neg" timestamp (fun x ->
        Timedesc.Span.(abs x >= zero))

  let add_commutative =
    QCheck.Test.make ~count:100_000 ~name:"add_commutative"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Timedesc.Span.(equal (x + y) (y + x)))

  let add_associative =
    QCheck.Test.make ~count:100_000 ~name:"add_associative"
      QCheck.(triple timestamp timestamp timestamp)
      (fun (x, y, z) -> Timedesc.Span.(equal (x + (y + z)) (x + y + z)))

  let add_identity =
    QCheck.Test.make ~count:100_000 ~name:"add_identity" timestamp (fun x ->
        Timedesc.Span.(equal (x + zero) (zero + x)))

  let neg_then_sub_is_same_as_add_then_neg =
    QCheck.Test.make ~count:100_000 ~name:"neg_then_sub_is_same_as_add_then_neg"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Timedesc.Span.(equal (neg x - y) (neg (x + y))))

  let neg_distributive1 =
    QCheck.Test.make ~count:100_000 ~name:"neg_distributive1"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Timedesc.Span.(equal (neg (x + y)) (neg x + neg y)))

  let neg_distributive2 =
    QCheck.Test.make ~count:100_000 ~name:"neg_distributive1"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Timedesc.Span.(equal (neg (x - y)) (neg x - neg y)))

  let add_neg_self =
    QCheck.Test.make ~count:100_000 ~name:"add_neg_self" timestamp (fun x ->
        Timedesc.Span.(equal zero (x + neg x)))

  let neg_add_self =
    QCheck.Test.make ~count:100_000 ~name:"neg_add_self" timestamp (fun x ->
        Timedesc.Span.(equal zero (neg x + x)))

  let to_of_float_s_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"to_of_float_is_accurate" timestamp
      (fun x ->
        let x' = Timedesc.Span.of_float_s @@ Timedesc.Span.to_float_s x in
        Timedesc.Span.(abs (x - x') < make ~s:1L ~ns:1000 ()))

  let floor_is_idempotent =
    QCheck.Test.make ~count:100_000 ~name:"floor_is_idempotent" timestamp
      (fun x -> Timedesc.Span.(equal (floor x) (floor @@ floor x)))

  let floor_result_is_le_original =
    QCheck.Test.make ~count:100_000 ~name:"floor_result_is_le_original"
      timestamp (fun x -> Timedesc.Span.(le (floor x) x))

  let ceil_is_idempotent =
    QCheck.Test.make ~count:100_000 ~name:"ceil_is_idempotent" timestamp
      (fun x -> Timedesc.Span.(equal (ceil x) (ceil @@ ceil x)))

  let ceil_result_is_ge_original =
    QCheck.Test.make ~count:100_000 ~name:"ceil_result_is_ge_original" timestamp
      (fun x -> Timedesc.Span.(ge (ceil x) x))

  let round_is_idempotent =
    QCheck.Test.make ~count:100_000 ~name:"round_is_idempotent" timestamp
      (fun x -> Timedesc.Span.(equal (round x) (round @@ round x)))

  let round_result_is_bounded_by_ceil_and_floor =
    QCheck.Test.make ~count:100_000
      ~name:"round_result_is_bounded_by_ceil_and_floor" timestamp (fun x ->
        let r = Timedesc.Span.round x in
        Timedesc.Span.(floor x <= r && r <= ceil x))

  let accessors =
    QCheck.Test.make ~count:100_000 ~name:"accessors" duration (fun duration ->
        let s, ns = Timedesc.Span.to_s_ns duration in
        let s' = Timedesc.Span.get_s duration in
        let ns' = Timedesc.Span.get_ns_offset duration in
        s = s' && ns = ns')

  let of_to_view =
    QCheck.Test.make ~count:100_000 ~name:"to_of_view" duration (fun duration ->
        let view = Timedesc.Span.For_human.view duration in
        Timedesc.Span.equal duration
          (Timedesc.Span.For_human.make_exn ~sign:view.sign ~days:view.days
             ~hours:view.hours ~minutes:view.minutes ~seconds:view.seconds
             ~ns:view.ns ()))

  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" timestamp (fun s ->
        let s' =
          s
          |> Timedesc.Span.to_sexp
          |> Timedesc.Span.of_sexp
          |> CCResult.get_exn
        in
        Timedesc.Span.equal s s')

  let suite =
    [
      make_is_lossless;
      make_result_ns_is_within_bound;
      normalize_is_idempotent;
      add_sub;
      sub_add;
      sub_self;
      double_neg;
      neg;
      abs;
      abs_is_non_neg;
      add_commutative;
      add_associative;
      add_identity;
      neg_then_sub_is_same_as_add_then_neg;
      neg_distributive1;
      neg_distributive2;
      add_neg_self;
      neg_add_self;
      to_of_float_s_is_accurate;
      floor_is_idempotent;
      floor_result_is_le_original;
      ceil_is_idempotent;
      ceil_result_is_ge_original;
      round_is_idempotent;
      round_result_is_bounded_by_ceil_and_floor;
      accessors;
      of_to_view;
      to_of_sexp;
    ]
end
