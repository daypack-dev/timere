open Test_utils

module Alco = struct
  let lt_case0 () =
    Alcotest.(check bool)
    "lt case0"
    true
    (Timedesc.ISO_week.(lt
    (make_exn ~year:2000 ~week:1)
    (make_exn ~year:2000 ~week:1)
    )
    )

  let le_case0 () =
    Alcotest.(check bool)
    "le case0"
    true
    (Timedesc.ISO_week.(le
    (make_exn ~year:2000 ~week:1)
    (make_exn ~year:2000 ~week:1)
    )
    )

  let le_case1 () =
    Alcotest.(check bool)
    "le case1"
    true
    (Timedesc.ISO_week.(le
    (make_exn ~year:2000 ~week:1)
    (make_exn ~year:2000 ~week:1)
    )
    )

  let gt_case0 () =
    Alcotest.(check bool)
    "gt case0"
    true
    (Timedesc.ISO_week.(lt
    (make_exn ~year:2000 ~week:1)
    (make_exn ~year:2000 ~week:1)
    )
    )

  let ge_case0 () =
    Alcotest.(check bool)
    "ge case0"
    true
    (Timedesc.ISO_week.(le
    (make_exn ~year:2000 ~week:1)
    (make_exn ~year:2000 ~week:1)
    )
    )

  let ge_case1 () =
    Alcotest.(check bool)
    "ge case1"
    true
    (Timedesc.ISO_week.(le
    (make_exn ~year:2000 ~week:1)
    (make_exn ~year:2000 ~week:1)
    )
    )

  let of_iso8601_case0 () =
    Alcotest.(check ym_testable)
      "same date"
      (Timedesc.Ym.of_iso8601_exn "1977-06")
      (Timedesc.Ym.make_exn ~year:1977 ~month:6)

  let suite =
    [
      Alcotest.test_case "lt_case0" `Quick lt_case0;
      Alcotest.test_case "le_case0" `Quick le_case0;
      Alcotest.test_case "le_case1" `Quick le_case1;
      Alcotest.test_case "gt_case0" `Quick gt_case0;
      Alcotest.test_case "ge_case0" `Quick ge_case0;
      Alcotest.test_case "ge_case1" `Quick ge_case1;
      Alcotest.test_case "of_iso8601_case0" `Quick of_iso8601_case0;
    ]
end

module Qc = struct
  let suite = []
end
