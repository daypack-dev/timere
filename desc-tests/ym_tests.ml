open Test_utils

module Alco = struct
  let lt_case0 () =
    Alcotest.(check bool)
      "less than" true
      Timedesc.Ym.(
        lt (make_exn ~year:2000 ~month:1) (make_exn ~year:2000 ~month:2))

  let lt_case1 () =
    Alcotest.(check bool)
      "less than" false
      Timedesc.Ym.(
        lt (make_exn ~year:2000 ~month:1) (make_exn ~year:2000 ~month:1))

  let lt_case2 () =
    Alcotest.(check bool)
      "less than" false
      Timedesc.Ym.(
        lt (make_exn ~year:2000 ~month:2) (make_exn ~year:2000 ~month:1))

  let le_case0 () =
    Alcotest.(check bool)
      "less than or equal to" true
      Timedesc.Ym.(
        le (make_exn ~year:2000 ~month:1) (make_exn ~year:2000 ~month:2))

  let le_case1 () =
    Alcotest.(check bool)
      "less than or equal to" true
      Timedesc.Ym.(
        le (make_exn ~year:2000 ~month:1) (make_exn ~year:2000 ~month:1))

  let le_case2 () =
    Alcotest.(check bool)
      "less than or equal to" false
      Timedesc.Ym.(
        le (make_exn ~year:2000 ~month:2) (make_exn ~year:2000 ~month:1))

  let gt_case0 () =
    Alcotest.(check bool)
      "greater than or equal to" true
      Timedesc.Ym.(
        gt (make_exn ~year:2000 ~month:2) (make_exn ~year:2000 ~month:1))

  let gt_case1 () =
    Alcotest.(check bool)
      "greater than or equal to" false
      Timedesc.Ym.(
        gt (make_exn ~year:2000 ~month:1) (make_exn ~year:2000 ~month:1))

  let gt_case2 () =
    Alcotest.(check bool)
      "greater than or equal to" false
      Timedesc.Ym.(
        gt (make_exn ~year:2000 ~month:1) (make_exn ~year:2000 ~month:2))

  let ge_case0 () =
    Alcotest.(check bool)
      "greater than or equal to" true
      Timedesc.Ym.(
        ge (make_exn ~year:2000 ~month:2) (make_exn ~year:2000 ~month:1))

  let ge_case1 () =
    Alcotest.(check bool)
      "greater than or equal to" true
      Timedesc.Ym.(
        ge (make_exn ~year:2000 ~month:1) (make_exn ~year:2000 ~month:1))

  let ge_case2 () =
    Alcotest.(check bool)
      "greater than or equal to" false
      Timedesc.Ym.(
        ge (make_exn ~year:2000 ~month:1) (make_exn ~year:2000 ~month:2))

  let of_iso8601_case0 () =
    Alcotest.(check ym_testable)
      "same date"
      (Timedesc.Ym.of_iso8601_exn "1977-06")
      (Timedesc.Ym.make_exn ~year:1977 ~month:6)

  let wrap_around_case0 () =
    let y = Timedesc.Ym.add ~months:1 (Timedesc.Ym.make_exn ~year:1977 ~month:12)
    in
    Alcotest.(check ym_testable)
      "same date"
      (Timedesc.Ym.make_exn ~year:1978 ~month:1)
      y

  let wrap_around_case1 () =
    let y = Timedesc.Ym.add ~months:13 (Timedesc.Ym.make_exn ~year:1977 ~month:12)
    in
    Alcotest.(check ym_testable)
      "same date"
      (Timedesc.Ym.make_exn ~year:1979 ~month:1)
      y

  let wrap_around_case2 () =
    let y = Timedesc.Ym.sub ~months:1 (Timedesc.Ym.make_exn ~year:1977 ~month:1)
    in
    Alcotest.(check ym_testable)
      "same date"
      (Timedesc.Ym.make_exn ~year:1976 ~month:12)
      y

  let wrap_around_case3 () =
    let y = Timedesc.Ym.sub ~months:13 (Timedesc.Ym.make_exn ~year:1977 ~month:1)
    in
    Alcotest.(check ym_testable)
      "same date"
      (Timedesc.Ym.make_exn ~year:1975 ~month:12)
      y

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
      Alcotest.test_case "wrap_around_case0" `Quick wrap_around_case0;
      Alcotest.test_case "wrap_around_case1" `Quick wrap_around_case1;
      Alcotest.test_case "wrap_around_case2" `Quick wrap_around_case2;
      Alcotest.test_case "wrap_around_case3" `Quick wrap_around_case3;
    ]
end

module Qc = struct
  let add_identity =
    QCheck.Test.make ~count:100_000 ~name:"add_identity" ym (fun (year, month) ->
        let x = Timedesc.Ym.make_exn ~year ~month in
        Timedesc.Ym.(equal (add ~months:0 x) x))

  let sub_identity =
    QCheck.Test.make ~count:100_000 ~name:"sub_identity" ym (fun (year, month) ->
        let x = Timedesc.Ym.make_exn ~year ~month in
        Timedesc.Ym.(equal (sub ~months:0 x) x))

  let add_sub =
    QCheck.Test.make ~count:100_000 ~name:"add_sub"
      QCheck.(pair ym small_int)
      (fun ((year, month), y) ->
         let x = Timedesc.Ym.make_exn ~year ~month in
         Timedesc.Ym.(equal x (sub ~months:y (add ~months:y x))))

  let sub_add =
    QCheck.Test.make ~count:100_000 ~name:"sub_add"
      QCheck.(pair ym small_int)
      (fun ((year, month), y) ->
         let x = Timedesc.Ym.make_exn ~year ~month in
         Timedesc.Ym.(equal x (add ~months:y (sub ~months:y x))))

  let add_diff =
    QCheck.Test.make ~count:100_000 ~name:"add_diff"
      QCheck.(pair ym iso_week)
      (fun ((year_x, month_x), (year_y, month_y)) ->
         let x = Timedesc.Ym.make_exn ~year:year_x ~month:month_x in
         let y = Timedesc.Ym.make_exn ~year:year_y ~month:month_y in
         let diff = Timedesc.Ym.diff_months x y in
         Timedesc.Ym.(equal x (add ~months:diff y)))

  let sub_diff =
    QCheck.Test.make ~count:100_000 ~name:"sub_diff"
      QCheck.(pair ym iso_week)
      (fun ((year_x, month_x), (year_y, month_y)) ->
         let x = Timedesc.Ym.make_exn ~year:year_x ~month:month_x in
         let y = Timedesc.Ym.make_exn ~year:year_y ~month:month_y in
         let diff = Timedesc.Ym.diff_months x y in
         Timedesc.Ym.(equal y (sub ~months:diff x)))

  let suite =
    [
      add_identity;
      sub_identity;
      add_sub;
      sub_add;
      add_diff;
      sub_diff;
    ]
end
