open Test_utils

module Alco = struct
  let week_date0 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd_date.make_exn ~year:1977 ~month:12 ~day:31)
      (Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:1977 ~iso_week:52
         ~weekday:`Sat)

  let week_date1 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd_date.make_exn ~year:1978 ~month:01 ~day:02)
      (Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:1978 ~iso_week:1
         ~weekday:`Mon)

  let week_date2 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd_date.make_exn ~year:1979 ~month:01 ~day:01)
      (Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:1979 ~iso_week:1
         ~weekday:`Mon)

  let week_date3 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd_date.make_exn ~year:1979 ~month:12 ~day:31)
      (Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:1980 ~iso_week:1
         ~weekday:`Mon)

  let week_date4 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd_date.make_exn ~year:1980 ~month:12 ~day:28)
      (Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:1980 ~iso_week:52
         ~weekday:`Sun)

  let week_date5 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.Ymd_date.make_exn ~year:1969 ~month:12 ~day:31)
      (Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:1970 ~iso_week:1
         ~weekday:`Wed)

  let of_iso8601_case0 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.of_iso8601_exn "1977-W52-6")
      (Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:1977 ~iso_week:52
         ~weekday:`Sat)

  let of_iso8601_case1 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.of_iso8601_exn "1970-W1-3")
      (Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:1970 ~iso_week:1
         ~weekday:`Wed)

  let of_iso8601_case2 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.of_iso8601_exn "1977-101")
      (Timedesc.Date.ISO_ord_date.make_exn ~year:1977 ~day_of_year:101)

  let of_iso8601_case3 () =
    Alcotest.(check date_testable)
      "same date"
      (Timedesc.Date.of_iso8601_exn "1969-132")
      (Timedesc.Date.ISO_ord_date.make_exn ~year:1969 ~day_of_year:132)

  let suite =
    [
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
  let view_is_same_as_original_iso_ord_date =
    QCheck.Test.make ~count:100_000
      ~name:"view_is_same_as_original_iso_ord_date" iso_ord_date
      (fun (year', day_of_year') ->
        let d =
          Timedesc.Date.ISO_ord_date.make_exn ~year:year'
            ~day_of_year:day_of_year'
        in
        let { Timedesc.Date.ISO_ord_date.year; day_of_year } =
          Timedesc.Date.ISO_ord_date.view d
        in
        year = year' && day_of_year = day_of_year')

  let view_is_same_as_original_iso_week_date =
    QCheck.Test.make ~count:100_000
      ~name:"view_is_same_as_original_iso_week_date" iso_week_date
      (fun (iso_week_year', iso_week', weekday') ->
        let d =
          Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:iso_week_year'
            ~iso_week:iso_week' ~weekday:weekday'
        in
        let { Timedesc.Date.ISO_week_date.iso_week_year; iso_week; weekday } =
          Timedesc.Date.ISO_week_date.view d
        in
        iso_week_year = iso_week_year'
        && iso_week = iso_week'
        && weekday = weekday')

  let view_is_same_as_original_ymd_date =
    QCheck.Test.make ~count:100_000 ~name:"view_is_same_as_original_ymd_date"
      ymd_date (fun (year', month', day') ->
        let d =
          Timedesc.Date.Ymd_date.make_exn ~year:year' ~month:month' ~day:day'
        in
        let { Timedesc.Date.Ymd_date.year; month; day } =
          Timedesc.Date.Ymd_date.view d
        in
        year = year' && month = month' && day = day')

  let iso_ord_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"iso_ord_date_accessors" iso_ord_date
      (fun (year', day_of_year') ->
        let d =
          Timedesc.Date.ISO_ord_date.make_exn ~year:year'
            ~day_of_year:day_of_year'
        in
        let year = Timedesc.Date.year d in
        let day_of_year = Timedesc.Date.day_of_year d in
        year = year' && day_of_year = day_of_year')

  let iso_week_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"iso_week_date_accessors"
      iso_week_date (fun (iso_week_year', iso_week', weekday') ->
        let d =
          Timedesc.Date.ISO_week_date.make_exn ~iso_week_year:iso_week_year'
            ~iso_week:iso_week' ~weekday:weekday'
        in
        let iso_week_year = Timedesc.Date.iso_week_year d in
        let iso_week = Timedesc.Date.iso_week d in
        let weekday = Timedesc.Date.weekday d in
        iso_week_year = iso_week_year'
        && iso_week = iso_week'
        && weekday = weekday')

  let ymd_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"ymd_date_accessors" ymd_date
      (fun (year', month', day') ->
        let d =
          Timedesc.Date.Ymd_date.make_exn ~year:year' ~month:month' ~day:day'
        in
        let year = Timedesc.Date.year d in
        let month = Timedesc.Date.month d in
        let day = Timedesc.Date.day d in
        year = year' && month = month' && day = day')

  let suite =
    [
      view_is_same_as_original_iso_ord_date;
      view_is_same_as_original_iso_week_date;
      view_is_same_as_original_ymd_date;
      iso_ord_date_accessors;
      iso_week_date_accessors;
      ymd_date_accessors;
    ]
end
