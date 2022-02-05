open Test_utils

module Alco = struct
  let lt_case0 () =
    Alcotest.(check bool)
      "less than" true
      Timedesc.Date.(
        lt
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:2))

  let lt_case1 () =
    Alcotest.(check bool)
      "less than" false
      Timedesc.Date.(
        lt
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1))

  let lt_case2 () =
    Alcotest.(check bool)
      "less than" false
      Timedesc.Date.(
        lt
          (Ymd.make_exn ~year:2000 ~month:1 ~day:2)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1))

  let le_case0 () =
    Alcotest.(check bool)
      "less than or equal to" true
      Timedesc.Date.(
        le
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:2))

  let le_case1 () =
    Alcotest.(check bool)
      "less than or equal to" true
      Timedesc.Date.(
        le
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1))

  let le_case2 () =
    Alcotest.(check bool)
      "less than or equal to" false
      Timedesc.Date.(
        le
          (Ymd.make_exn ~year:2000 ~month:1 ~day:2)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1))

  let gt_case0 () =
    Alcotest.(check bool)
      "greater than" true
      Timedesc.Date.(
        gt
          (Ymd.make_exn ~year:2000 ~month:1 ~day:2)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1))

  let gt_case1 () =
    Alcotest.(check bool)
      "greater than" false
      Timedesc.Date.(
        gt
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1))

  let gt_case2 () =
    Alcotest.(check bool)
      "greater than" false
      Timedesc.Date.(
        gt
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:2))

  let ge_case0 () =
    Alcotest.(check bool)
      "greater than or equal to" true
      Timedesc.Date.(
        ge
          (Ymd.make_exn ~year:2000 ~month:1 ~day:2)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1))

  let ge_case1 () =
    Alcotest.(check bool)
      "greater than or equal to" true
      Timedesc.Date.(
        ge
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1))

  let ge_case2 () =
    Alcotest.(check bool)
      "greater than or equal to" false
      Timedesc.Date.(
        ge
          (Ymd.make_exn ~year:2000 ~month:1 ~day:1)
          (Ymd.make_exn ~year:2000 ~month:1 ~day:2))

  let week_date0 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd.make_exn ~year:1977 ~month:12 ~day:31)
      (Timedesc.Date.ISO_week_date.make_exn ~year:1977 ~week:52 ~weekday:`Sat)

  let week_date1 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd.make_exn ~year:1978 ~month:01 ~day:02)
      (Timedesc.Date.ISO_week_date.make_exn ~year:1978 ~week:1 ~weekday:`Mon)

  let week_date2 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd.make_exn ~year:1979 ~month:01 ~day:01)
      (Timedesc.Date.ISO_week_date.make_exn ~year:1979 ~week:1 ~weekday:`Mon)

  let week_date3 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd.make_exn ~year:1979 ~month:12 ~day:31)
      (Timedesc.Date.ISO_week_date.make_exn ~year:1980 ~week:1 ~weekday:`Mon)

  let week_date4 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd.make_exn ~year:1980 ~month:12 ~day:28)
      (Timedesc.Date.ISO_week_date.make_exn ~year:1980 ~week:52 ~weekday:`Sun)

  let week_date5 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd.make_exn ~year:1969 ~month:12 ~day:31)
      (Timedesc.Date.ISO_week_date.make_exn ~year:1970 ~week:1 ~weekday:`Wed)

  let of_iso8601_case0 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.of_iso8601_exn "1977-W52-6")
      (Timedesc.Date.ISO_week_date.make_exn ~year:1977 ~week:52 ~weekday:`Sat)

  let of_iso8601_case1 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.of_iso8601_exn "1970-W1-3")
      (Timedesc.Date.ISO_week_date.make_exn ~year:1970 ~week:1 ~weekday:`Wed)

  let of_iso8601_case2 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.of_iso8601_exn "1977-101")
      (Timedesc.Date.ISO_ord.make_exn ~year:1977 ~day_of_year:101)

  let of_iso8601_case3 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.of_iso8601_exn "1969-132")
      (Timedesc.Date.ISO_ord.make_exn ~year:1969 ~day_of_year:132)

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
      Alcotest.test_case "week_date0" `Quick week_date0;
      Alcotest.test_case "week_date1" `Quick week_date1;
      Alcotest.test_case "week_date2" `Quick week_date2;
      Alcotest.test_case "week_date3" `Quick week_date3;
      Alcotest.test_case "week_date4" `Quick week_date4;
      Alcotest.test_case "of_iso8601_case0" `Quick of_iso8601_case0;
      Alcotest.test_case "of_iso8601_case1" `Quick of_iso8601_case1;
      Alcotest.test_case "of_iso8601_case2" `Quick of_iso8601_case2;
      Alcotest.test_case "of_iso8601_case3" `Quick of_iso8601_case3;
    ]
end

module Qc = struct
  let to_rfc3339_of_iso8601 =
    QCheck.Test.make ~count:100_000 ~name:"to_rfc3339_of_iso8601" ymd_date
      (fun (year, month, day) ->
         let d = Timedesc.Date.Ymd.make_exn ~year ~month ~day in
         let d' =
           d
           |> Timedesc.Date.to_rfc3339
           |> Timedesc.Date.of_iso8601
           |> CCResult.get_exn
         in
         Timedesc.Date.equal d d')

  let ymd_to_iso8601_of_iso8601 =
    QCheck.Test.make ~count:100_000 ~name:"ymd_to_iso8601_of_iso8601" ymd_date
      (fun (year, month, day) ->
         let d = Timedesc.Date.Ymd.make_exn ~year ~month ~day in
         let d' =
           d
           |> Timedesc.Date.Ymd.to_iso8601
           |> Timedesc.Date.Ymd.of_iso8601
           |> CCResult.get_exn
         in
         Timedesc.Date.equal d d')

  let iso_week_date_to_iso8601_of_iso8601 =
    QCheck.Test.make ~count:100_000 ~name:"iso_week_date_to_iso8601_of_iso8601"
      iso_week_date (fun (year, week, weekday) ->
          let d = Timedesc.Date.ISO_week_date.make_exn ~year ~week ~weekday in
          let d' =
            d
            |> Timedesc.Date.ISO_week_date.to_iso8601
            |> Timedesc.Date.ISO_week_date.of_iso8601
            |> CCResult.get_exn
          in
          Timedesc.Date.equal d d')

  let iso_ord_to_iso8601_of_iso8601 =
    QCheck.Test.make ~count:100_000 ~name:"iso_ord_to_iso8601_of_iso8601"
      iso_ord_date (fun (year, day_of_year) ->
          let d = Timedesc.Date.ISO_ord.make_exn ~year ~day_of_year in
          let d' =
            d
            |> Timedesc.Date.ISO_ord.to_iso8601
            |> Timedesc.Date.ISO_ord.of_iso8601
            |> CCResult.get_exn
          in
          Timedesc.Date.equal d d')

  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" ymd_date
      (fun (year, month, day) ->
         let d = Timedesc.Date.Ymd.make_exn ~year ~month ~day in
         let d' =
           d
           |> Timedesc.Date.to_sexp
           |> Timedesc.Date.of_sexp
           |> CCResult.get_exn
         in
         Timedesc.Date.equal d d')

  let view_is_same_as_original_iso_ord_date =
    QCheck.Test.make ~count:100_000
      ~name:"view_is_same_as_original_iso_ord_date" iso_ord_date
      (fun (year', day_of_year') ->
         let d =
           Timedesc.Date.ISO_ord.make_exn ~year:year' ~day_of_year:day_of_year'
         in
         let { Timedesc.Date.ISO_ord.year; day_of_year } =
           Timedesc.Date.ISO_ord.view d
         in
         year = year' && day_of_year = day_of_year')

  let view_is_same_as_original_iso_week_date =
    QCheck.Test.make ~count:100_000
      ~name:"view_is_same_as_original_iso_week_date" iso_week_date
      (fun (year', week', weekday') ->
         let d =
           Timedesc.Date.ISO_week_date.make_exn ~year:year' ~week:week'
             ~weekday:weekday'
         in
         let { Timedesc.Date.ISO_week_date.year; week; weekday } =
           Timedesc.Date.ISO_week_date.view d
         in
         year = year' && week = week' && weekday = weekday')

  let view_is_same_as_original_ymd_date =
    QCheck.Test.make ~count:100_000 ~name:"view_is_same_as_original_ymd_date"
      ymd_date (fun (year', month', day') ->
          let d =
            Timedesc.Date.Ymd.make_exn ~year:year' ~month:month' ~day:day'
          in
          let { Timedesc.Date.Ymd.year; month; day } = Timedesc.Date.Ymd.view d in
          year = year' && month = month' && day = day')

  let iso_ord_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"iso_ord_date_accessors" iso_ord_date
      (fun (year', day_of_year') ->
         let d =
           Timedesc.Date.ISO_ord.make_exn ~year:year' ~day_of_year:day_of_year'
         in
         let year = Timedesc.Date.year d in
         let day_of_year = Timedesc.Date.day_of_year d in
         year = year' && day_of_year = day_of_year')

  let iso_week_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"iso_week_date_accessors"
      iso_week_date (fun (year', week', weekday') ->
          let d =
            Timedesc.Date.ISO_week_date.make_exn ~year:year' ~week:week'
              ~weekday:weekday'
          in
          let year = Timedesc.Date.iso_year d in
          let year'', week =
            Timedesc.ISO_week.year_week @@ Timedesc.Date.iso_week d
          in
          let weekday = Timedesc.Date.weekday d in
          year = year' && year = year'' && week = week' && weekday = weekday')

  let ymd_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"ymd_date_accessors" ymd_date
      (fun (year', month', day') ->
         let d =
           Timedesc.Date.Ymd.make_exn ~year:year' ~month:month' ~day:day'
         in
         let year = Timedesc.Date.year d in
         let month = Timedesc.Date.month d in
         let day = Timedesc.Date.day d in
         year = year' && month = month' && day = day')

  let add_identity =
    QCheck.Test.make ~count:100_000 ~name:"add_identity" ymd_date (fun (year, month, day) ->
        let x = Timedesc.Date.Ymd.make_exn ~year ~month ~day in
        Timedesc.Date.(equal (add ~days:0 x) x))

  let sub_identity =
    QCheck.Test.make ~count:100_000 ~name:"sub_identity" ymd_date (fun (year, month, day) ->
        let x = Timedesc.Date.Ymd.make_exn ~year ~month ~day in
        Timedesc.Date.(equal (sub ~days:0 x) x))

  let add_sub =
    QCheck.Test.make ~count:100_000 ~name:"add_sub"
      QCheck.(pair ymd_date small_int)
      (fun ((year, month, day), y) ->
         let x = Timedesc.Date.Ymd.make_exn ~year ~month ~day in
         Timedesc.Date.(equal x (sub ~days:y (add ~days:y x))))

  let sub_add =
    QCheck.Test.make ~count:100_000 ~name:"sub_add"
      QCheck.(pair ymd_date small_int)
      (fun ((year, month, day), y) ->
         let x = Timedesc.Date.Ymd.make_exn ~year ~month ~day in
         Timedesc.Date.(equal x (add ~days:y (sub ~days:y x))))

  let add_diff =
    QCheck.Test.make ~count:100_000 ~name:"add_diff"
      QCheck.(pair ymd_date ymd_date)
      (fun ((year_x, month_x, day_x), (year_y, month_y, day_y)) ->
         let x = Timedesc.Date.Ymd.make_exn ~year:year_x ~month:month_x ~day:day_x in
         let y = Timedesc.Date.Ymd.make_exn ~year:year_y ~month:month_y ~day:day_y in
         let diff = Timedesc.Date.diff_days x y in
         Timedesc.Date.(equal y (add ~days:diff x)))

  let sub_diff =
    QCheck.Test.make ~count:100_000 ~name:"add_diff"
      QCheck.(pair ymd_date ymd_date)
      (fun ((year_x, month_x, day_x), (year_y, month_y, day_y)) ->
         let x = Timedesc.Date.Ymd.make_exn ~year:year_x ~month:month_x ~day:day_x in
         let y = Timedesc.Date.Ymd.make_exn ~year:year_y ~month:month_y ~day:day_y in
         let diff = Timedesc.Date.diff_days x y in
         Timedesc.Date.(equal x (sub ~days:diff y)))

  let suite =
    [
      to_rfc3339_of_iso8601;
      iso_week_date_to_iso8601_of_iso8601;
      iso_ord_to_iso8601_of_iso8601;
      ymd_to_iso8601_of_iso8601;
      to_of_sexp;
      view_is_same_as_original_iso_ord_date;
      view_is_same_as_original_iso_week_date;
      view_is_same_as_original_ymd_date;
      iso_ord_date_accessors;
      iso_week_date_accessors;
      ymd_date_accessors;
      add_identity;
      sub_identity;
      add_sub;
      sub_add;
      add_diff;
      sub_diff;
    ]
end
