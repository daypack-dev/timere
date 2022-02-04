open Test_utils

module Alco = struct
  let lt_case0 () =
    Alcotest.(check bool)
      "less than" true
      Timedesc.ISO_week.(
        lt (make_exn ~year:2000 ~week:1) (make_exn ~year:2000 ~week:2))

  let lt_case1 () =
    Alcotest.(check bool)
      "less than" false
      Timedesc.ISO_week.(
        lt (make_exn ~year:2000 ~week:1) (make_exn ~year:2000 ~week:1))

  let lt_case2 () =
    Alcotest.(check bool)
      "less than" false
      Timedesc.ISO_week.(
        lt (make_exn ~year:2000 ~week:2) (make_exn ~year:2000 ~week:1))

  let le_case0 () =
    Alcotest.(check bool)
      "less than or equal to" true
      Timedesc.ISO_week.(
        le (make_exn ~year:2000 ~week:1) (make_exn ~year:2000 ~week:2))

  let le_case1 () =
    Alcotest.(check bool)
      "less than or equal to" true
      Timedesc.ISO_week.(
        le (make_exn ~year:2000 ~week:1) (make_exn ~year:2000 ~week:1))

  let le_case2 () =
    Alcotest.(check bool)
      "less than or equal to" false
      Timedesc.ISO_week.(
        le (make_exn ~year:2000 ~week:2) (make_exn ~year:2000 ~week:1))

  let gt_case0 () =
    Alcotest.(check bool)
      "greater than" true
      Timedesc.ISO_week.(
        gt (make_exn ~year:2000 ~week:2) (make_exn ~year:2000 ~week:1))

  let gt_case1 () =
    Alcotest.(check bool)
      "greater than" false
      Timedesc.ISO_week.(
        gt (make_exn ~year:2000 ~week:1) (make_exn ~year:2000 ~week:1))

  let gt_case2 () =
    Alcotest.(check bool)
      "greater than" false
      Timedesc.ISO_week.(
        gt (make_exn ~year:2000 ~week:1) (make_exn ~year:2000 ~week:2))

  let ge_case0 () =
    Alcotest.(check bool)
      "greater than or equal to" true
      Timedesc.ISO_week.(
        ge (make_exn ~year:2000 ~week:2) (make_exn ~year:2000 ~week:1))

  let ge_case1 () =
    Alcotest.(check bool)
      "greater than or equal to" true
      Timedesc.ISO_week.(
        ge (make_exn ~year:2000 ~week:1) (make_exn ~year:2000 ~week:1))

  let ge_case2 () =
    Alcotest.(check bool)
      "greater than or equal to" false
      Timedesc.ISO_week.(
        ge (make_exn ~year:2000 ~week:1) (make_exn ~year:2000 ~week:2))

  let of_iso8601_case0 () =
    Alcotest.(check ym_testable)
      "same date"
      (Timedesc.Ym.of_iso8601_exn "1977-06")
      (Timedesc.Ym.make_exn ~year:1977 ~month:6)

  let suite =
    [
      Alcotest.test_case "lt_case0" `Quick lt_case0;
      Alcotest.test_case "lt_case1" `Quick lt_case1;
      Alcotest.test_case "lt_case2" `Quick lt_case2;
      Alcotest.test_case "le_case0" `Quick le_case0;
      Alcotest.test_case "le_case1" `Quick le_case1;
      Alcotest.test_case "le_case2" `Quick le_case2;
      Alcotest.test_case "gt_case0" `Quick gt_case0;
      Alcotest.test_case "gt_case1" `Quick gt_case1;
      Alcotest.test_case "gt_case2" `Quick gt_case2;
      Alcotest.test_case "ge_case0" `Quick ge_case0;
      Alcotest.test_case "ge_case1" `Quick ge_case1;
      Alcotest.test_case "ge_case2" `Quick ge_case2;
      Alcotest.test_case "of_iso8601_case0" `Quick of_iso8601_case0;
    ]
end

module Qc = struct
  let suite = []
end
