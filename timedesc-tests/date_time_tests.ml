open Test_utils

module Alco = struct
  let leap_second0 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:2020 ~month:1 ~day:1
         ~hour:0 ~minute:0 ~second:60 ~ns:1_000_000 ()
       |> Timedesc.to_timestamp_single)
      (Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:2020 ~month:1 ~day:1
         ~hour:0 ~minute:0 ~second:59 ~ns:1_000_000 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_leap_second0 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_iso8601 "2020-01-01T00:00:60.001Z")
      (Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:2020 ~month:1 ~day:1
         ~hour:0 ~minute:0 ~second:60 ~ns:1_000_000 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_leap_second1 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_iso8601 "2020-01-01T00:00:60.1Z")
      (Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:2020 ~month:1 ~day:1
         ~hour:0 ~minute:0 ~second:60 ~ns:100_000_000 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_leap_second_to_rfc3339_case0 () =
    Alcotest.(check string)
      "same timestamp" "2020-01-01T00:00:60Z"
      (Timedesc.of_iso8601 "2020-01-01T00:00:60Z"
       |> CCResult.get_exn
       |> Timedesc.to_rfc3339)

  let of_iso8601_leap_second_to_rfc3339_case1 () =
    Alcotest.(check string)
      "same timestamp" "2020-01-01T00:00:60.12305Z"
      (Timedesc.of_iso8601 "2020-01-01T00:00:60.12305Z"
       |> CCResult.get_exn
       |> Timedesc.to_rfc3339)

  let of_iso8601_case0 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn @@ Timedesc.Timestamp.of_iso8601 "2020-01-01T24:00:00Z")
      (Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:2020 ~month:1 ~day:1
         ~hour:23 ~minute:59 ~second:59 ~ns:999_999_999 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_case1 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn @@ Timedesc.Timestamp.of_iso8601 "1979-05-27T07:32:00Z")
      (Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:1979 ~month:5 ~day:27
         ~hour:7 ~minute:32 ~second:0 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_case2 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_iso8601 "1979-05-27T07:32:00-07:00")
      (Timedesc.make_exn
         ~tz:
           (Timedesc.Time_zone.make_offset_only_exn
              (Timedesc.Span.For_human.make_exn ~sign:`Neg ~hours:7 ()))
         ~year:1979 ~month:5 ~day:27 ~hour:7 ~minute:32 ~second:0 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_case3 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_iso8601 "1979-05-27T07:32:00.999999-07:00")
      (Timedesc.make_exn
         ~tz:
           (Timedesc.Time_zone.make_offset_only_exn
              (Timedesc.Span.For_human.make_exn ~sign:`Neg ~hours:7 ()))
         ~year:1979 ~month:5 ~day:27 ~hour:7 ~minute:32 ~second:0
         ~ns:999_999_000 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_case4 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_iso8601 "1979-05-27T07:32:00.999999999-07:00")
      (Timedesc.make_exn
         ~tz:
           (Timedesc.Time_zone.make_offset_only_exn
              (Timedesc.Span.For_human.make_exn ~sign:`Neg ~hours:7 ()))
         ~year:1979 ~month:5 ~day:27 ~hour:7 ~minute:32 ~second:0
         ~ns:999_999_999 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_case5 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_iso8601 "1979-05-27T07:32:00.000999999-07:00")
      (Timedesc.make_exn
         ~tz:
           (Timedesc.Time_zone.make_offset_only_exn
              (Timedesc.Span.For_human.make_exn ~sign:`Neg ~hours:7 ()))
         ~year:1979 ~month:5 ~day:27 ~hour:7 ~minute:32 ~second:0 ~ns:999_999 ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_case6 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_iso8601 "1969-12-31T23:59:59.000999999+00:00")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:1969 ~month:12 ~day:31 ~hour:23 ~minute:59 ~second:59 ~ns:999_999
         ()
       |> Timedesc.to_timestamp_single)

  let of_iso8601_case7 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_iso8601 "1910-02-28T02:59:57.000000999-07:30")
      (Timedesc.make_exn
         ~tz:
           (Timedesc.Time_zone.make_offset_only_exn
              (Timedesc.Span.For_human.make_exn ~sign:`Neg ~hours:7 ~minutes:30
                 ()))
         ~year:1910 ~month:2 ~day:28 ~hour:2 ~minute:59 ~second:57 ~ns:999 ()
       |> Timedesc.to_timestamp_single)

  let maybe_zoneless_of_iso8601_case0 () =
    Alcotest.(check maybe_zoneless_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Zoneless.maybe_zoneless_of_iso8601 "2020-01-01T24:00:00Z")
      (`Zoned
         (Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:2020 ~month:1 ~day:1
            ~hour:23 ~minute:59 ~second:59 ~ns:999_999_999 ()))

  let maybe_zoneless_of_iso8601_case1 () =
    Alcotest.(check maybe_zoneless_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Zoneless.maybe_zoneless_of_iso8601 "1910-05-27T07:32:00Z")
      (`Zoned
         (Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:1910 ~month:5
            ~day:27 ~hour:7 ~minute:32 ~second:0 ()))

  let maybe_zoneless_of_iso8601_case2 () =
    Alcotest.(check maybe_zoneless_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Zoneless.maybe_zoneless_of_iso8601 "2020-01-01T24:00:00")
      (`Zoneless
         (Timedesc.Zoneless.make
            (Timedesc.Date.Ymd.make_exn ~year:2020 ~month:1 ~day:1)
            (Timedesc.Time.make_exn ~hour:23 ~minute:59 ~second:59
               ~ns:999_999_999 ())))

  let maybe_zoneless_of_iso8601_case3 () =
    Alcotest.(check maybe_zoneless_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Zoneless.maybe_zoneless_of_iso8601 "1910-05-27T07:32:00")
      (`Zoneless
         (Timedesc.Zoneless.make
            (Timedesc.Date.Ymd.make_exn ~year:1910 ~month:5 ~day:27)
            (Timedesc.Time.make_exn ~hour:7 ~minute:32 ~second:0 ())))

  let zoneless_of_iso8601_case0 () =
    Alcotest.(check zoneless_testable)
      "same timestamp"
      (CCResult.get_exn @@ Timedesc.Zoneless.of_iso8601 "2020-01-01T24:00:00")
      (Timedesc.Zoneless.make
         (Timedesc.Date.Ymd.make_exn ~year:2020 ~month:1 ~day:1)
         (Timedesc.Time.make_exn ~hour:23 ~minute:59 ~second:59 ~ns:999_999_999
            ()))

  let zoneless_of_iso8601_case1 () =
    Alcotest.(check zoneless_testable)
      "same timestamp"
      (CCResult.get_exn @@ Timedesc.Zoneless.of_iso8601 "1910-05-27T07:32:00")
      (Timedesc.Zoneless.make
         (Timedesc.Date.Ymd.make_exn ~year:1910 ~month:5 ~day:27)
         (Timedesc.Time.make_exn ~hour:7 ~minute:32 ~second:0 ()))

  let to_rfc3339_case0 () =
    Alcotest.(check string)
      "same string" "1979-05-27T07:32:00Z"
      (Timedesc.Timestamp.to_rfc3339 (Timedesc.Span.make ~s:296638320L ()))

  let to_rfc3339_case1 () =
    Alcotest.(check string)
      "same string" "1979-05-27T07:32:00.999999Z"
      (Timedesc.Timestamp.to_rfc3339
         (Timedesc.Span.make ~s:296638320L ~ns:999_999_000 ()))

  let to_rfc3339_case2 () =
    Alcotest.(check string)
      "same string" "1979-05-27T07:32:00.999999999Z"
      (Timedesc.Timestamp.to_rfc3339
         (Timedesc.Span.make ~s:296638320L ~ns:999_999_999 ()))

  let to_rfc3339_case3 () =
    Alcotest.(check string)
      "same string" "1979-05-27T07:32:00.000999999Z"
      (Timedesc.Timestamp.to_rfc3339
         (Timedesc.Span.make ~s:296638320L ~ns:999_999 ()))

  let to_rfc3339_case4 () =
    Alcotest.(check string)
      "same string" "1969-12-31T23:59:59Z"
      (Timedesc.Timestamp.to_rfc3339 (Timedesc.Span.make ~s:(-1L) ~ns:0 ()))

  let to_rfc3339_case5 () =
    Alcotest.(check string)
      "same string" "1969-12-31T23:59:59.000000999Z"
      (Timedesc.Timestamp.to_rfc3339 (Timedesc.Span.make ~s:(-1L) ~ns:999 ()))

  let to_rfc3339_case6 () =
    Alcotest.(check string)
      "same string" "1910-02-28T02:59:59.000000999Z"
      (Timedesc.Timestamp.to_rfc3339
         (Timedesc.Span.make ~s:(-1888434001L) ~ns:999 ()))

  let of_rfc9110_imf_fixdate_case0 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Wed, 01 Jan 2020 23:59:59 GMT")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:2020 ~month:1 ~day:1 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let of_rfc9110_imf_fixdate_case1 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Sun, 27 May 1979 23:59:59 GMT")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:1979 ~month:5 ~day:27 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let of_rfc9110_imf_fixdate_case2 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Wed, 31 Dec 1969 23:59:59 GMT")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:1969 ~month:12 ~day:31 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let of_rfc9110_rfc850_date_case0 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Wednesday, 01-Jan-20 23:59:59 GMT")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:2020 ~month:1 ~day:1 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let of_rfc9110_rfc850_date_case1 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Sunday, 27-May-79 23:59:59 GMT")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:1979 ~month:5 ~day:27 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let of_rfc9110_rfc850_date_case2 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Wednesday, 31-Dec-69 23:59:59 GMT")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:1969 ~month:12 ~day:31 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let of_rfc9110_asctime_date_case0 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Wed Jan  1 23:59:59 2020")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:2020 ~month:1 ~day:1 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let of_rfc9110_asctime_date_case1 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Sun May 27 23:59:59 1979")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:1979 ~month:5 ~day:27 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let of_rfc9110_asctime_date_case2 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (CCResult.get_exn
       @@ Timedesc.Timestamp.of_rfc9110 "Wed Dec 31 23:59:59 1969")
      (Timedesc.make_exn
         ~tz:(Timedesc.Time_zone.make_offset_only_exn Timedesc.Span.zero)
         ~year:1969 ~month:12 ~day:31 ~hour:23 ~minute:59 ~second:59 ~ns:0
         ()
       |> Timedesc.to_timestamp_single)

  let to_rfc9110_case0 () =
    Alcotest.(check string)
      "same string" "Sun, 27 May 1979 07:32:00 GMT"
      (Timedesc.Timestamp.to_rfc9110 (Timedesc.Span.make ~s:296638320L ()))

  let to_rfc9110_case1 () =
    Alcotest.(check string)
      "same string" "Sun, 27 May 1979 07:32:00 GMT"
      (Timedesc.Timestamp.to_rfc9110
         (Timedesc.Span.make ~s:296638320L ~ns:999_999_000 ()))

  let to_rfc9110_case2 () =
    Alcotest.(check string)
      "same string" "Sun, 27 May 1979 07:32:00 GMT"
      (Timedesc.Timestamp.to_rfc9110
         (Timedesc.Span.make ~s:296638320L ~ns:999_999_999 ()))

  let to_rfc9110_case3 () =
    Alcotest.(check string)
      "same string" "Sun, 27 May 1979 07:32:00 GMT"
      (Timedesc.Timestamp.to_rfc9110
         (Timedesc.Span.make ~s:296638320L ~ns:999_999 ()))

  let to_rfc9110_case4 () =
    Alcotest.(check string)
      "same string" "Wed, 31 Dec 1969 23:59:59 GMT"
      (Timedesc.Timestamp.to_rfc9110 (Timedesc.Span.make ~s:(-1L) ~ns:0 ()))

  let to_rfc9110_case5 () =
    Alcotest.(check string)
      "same string" "Wed, 31 Dec 1969 23:59:59 GMT"
      (Timedesc.Timestamp.to_rfc9110 (Timedesc.Span.make ~s:(-1L) ~ns:999 ()))

  let to_rfc9110_case6 () =
    Alcotest.(check string)
      "same string" "Mon, 28 Feb 1910 02:59:59 GMT"
      (Timedesc.Timestamp.to_rfc9110
         (Timedesc.Span.make ~s:(-1888434001L) ~ns:999 ()))

  let suite =
    [
      Alcotest.test_case "leap_second0" `Quick leap_second0;
      Alcotest.test_case "of_iso8601_leap_second0" `Quick
        of_iso8601_leap_second0;
      Alcotest.test_case "of_iso8601_leap_second1" `Quick
        of_iso8601_leap_second1;
      Alcotest.test_case "of_iso8601_leap_second_to_rfc3339_case0" `Quick
        of_iso8601_leap_second_to_rfc3339_case0;
      Alcotest.test_case "of_iso8601_leap_second_to_rfc3339_case1" `Quick
        of_iso8601_leap_second_to_rfc3339_case1;
      Alcotest.test_case "of_iso8601_case0" `Quick of_iso8601_case0;
      Alcotest.test_case "of_iso8601_case1" `Quick of_iso8601_case1;
      Alcotest.test_case "of_iso8601_case2" `Quick of_iso8601_case2;
      Alcotest.test_case "of_iso8601_case3" `Quick of_iso8601_case3;
      Alcotest.test_case "of_iso8601_case4" `Quick of_iso8601_case4;
      Alcotest.test_case "of_iso8601_case5" `Quick of_iso8601_case5;
      Alcotest.test_case "of_iso8601_case6" `Quick of_iso8601_case6;
      Alcotest.test_case "of_iso8601_case7" `Quick of_iso8601_case7;
      Alcotest.test_case "maybe_zoneless_of_iso8601_case0" `Quick
        maybe_zoneless_of_iso8601_case0;
      Alcotest.test_case "maybe_zoneless_of_iso8601_case1" `Quick
        maybe_zoneless_of_iso8601_case1;
      Alcotest.test_case "maybe_zoneless_of_iso8601_case2" `Quick
        maybe_zoneless_of_iso8601_case2;
      Alcotest.test_case "maybe_zoneless_of_iso8601_case3" `Quick
        maybe_zoneless_of_iso8601_case3;
      Alcotest.test_case "zoneless_of_iso8601_case0" `Quick
        zoneless_of_iso8601_case0;
      Alcotest.test_case "zoneless_of_iso8601_case1" `Quick
        zoneless_of_iso8601_case1;
      Alcotest.test_case "to_rfc3339_case0" `Quick to_rfc3339_case0;
      Alcotest.test_case "to_rfc3339_case1" `Quick to_rfc3339_case1;
      Alcotest.test_case "to_rfc3339_case2" `Quick to_rfc3339_case2;
      Alcotest.test_case "to_rfc3339_case3" `Quick to_rfc3339_case3;
      Alcotest.test_case "to_rfc3339_case4" `Quick to_rfc3339_case4;
      Alcotest.test_case "to_rfc3339_case5" `Quick to_rfc3339_case5;
      Alcotest.test_case "to_rfc3339_case6" `Quick to_rfc3339_case6;
      Alcotest.test_case "of_rfc9110_imf_fixdate_case0" `Quick of_rfc9110_imf_fixdate_case0;
      Alcotest.test_case "of_rfc9110_imf_fixdate_case1" `Quick of_rfc9110_imf_fixdate_case1;
      Alcotest.test_case "of_rfc9110_imf_fixdate_case2" `Quick of_rfc9110_imf_fixdate_case2;
      Alcotest.test_case "of_rfc9110_rfc850_date_case0" `Quick of_rfc9110_rfc850_date_case0;
      Alcotest.test_case "of_rfc9110_rfc850_date_case1" `Quick of_rfc9110_rfc850_date_case1;
      Alcotest.test_case "of_rfc9110_rfc850_date_case2" `Quick of_rfc9110_rfc850_date_case2;
      Alcotest.test_case "of_rfc9110_asctime_date_case0" `Quick of_rfc9110_asctime_date_case0;
      Alcotest.test_case "of_rfc9110_asctime_date_case1" `Quick of_rfc9110_asctime_date_case1;
      Alcotest.test_case "of_rfc9110_asctime_date_case2" `Quick of_rfc9110_asctime_date_case2;
      Alcotest.test_case "to_rfc9110_case0" `Quick to_rfc9110_case0;
      Alcotest.test_case "to_rfc9110_case1" `Quick to_rfc9110_case1;
      Alcotest.test_case "to_rfc9110_case2" `Quick to_rfc9110_case2;
      Alcotest.test_case "to_rfc9110_case3" `Quick to_rfc9110_case3;
      Alcotest.test_case "to_rfc9110_case4" `Quick to_rfc9110_case4;
      Alcotest.test_case "to_rfc9110_case5" `Quick to_rfc9110_case5;
      Alcotest.test_case "to_rfc9110_case6" `Quick to_rfc9110_case6;
    ]
end

module Qc = struct
  let to_rfc3339_nano_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"to_rfc3339_nano_of_iso8601_is_lossless" date_time (fun dt ->
          let r =
            CCResult.get_exn
            @@ Timedesc.of_iso8601
            @@ Timedesc.to_rfc3339 ~frac_s:9 dt
          in
          Timedesc.(Span.equal (to_timestamp_single r) (to_timestamp_single dt))
        )

  let to_rfc3339_w_default_frac_s_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"to_rfc3339_w_default_frac_s_of_iso8601_is_lossless" date_time
      (fun dt ->
         let r =
           CCResult.get_exn
           @@ Timedesc.of_iso8601
           @@ Timedesc.to_rfc3339 dt
         in
         Timedesc.(Span.equal (to_timestamp_single r) (to_timestamp_single dt))
      )

  let to_rfc3339_of_iso8601_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"to_rfc3339_of_iso8601_is_accurate"
      QCheck.(pair (int_bound 9) date_time)
      (fun (frac_s, dt) ->
         let r =
           CCResult.get_exn
           @@ Timedesc.of_iso8601
           @@ Timedesc.to_rfc3339 ~frac_s dt
         in
         let r = Timedesc.to_timestamp_single r in
         let timestamp = Timedesc.to_timestamp_single dt in
         Timedesc.Span.(
           abs (r - timestamp)
           < make ~s:0L
             ~ns:(int_of_float (10. ** float_of_int (CCInt.sub 9 frac_s)))
             ()))

  let timestamp_to_rfc3339_nano_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"timestamp_to_rfc3339_nano_of_iso8601_is_lossless" timestamp (fun timestamp ->
          let r =
            CCResult.get_exn
            @@ Timedesc.Timestamp.of_iso8601
            @@ Timedesc.Timestamp.to_rfc3339 ~frac_s:9 timestamp
          in
          Timedesc.Span.equal r timestamp)

  let timestamp_to_rfc3339_w_default_frac_s_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"timestamp_to_rfc3339_w_default_frac_s_of_iso8601_is_lossless" timestamp
      (fun timestamp ->
         let r =
           CCResult.get_exn
           @@ Timedesc.Timestamp.of_iso8601
           @@ Timedesc.Timestamp.to_rfc3339 timestamp
         in
         Timedesc.Span.equal r timestamp)

  let timestamp_to_rfc3339_of_iso8601_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"timestamp_to_rfc3339_of_iso8601_is_accurate"
      QCheck.(pair (int_bound 9) timestamp)
      (fun (frac_s, timestamp) ->
         let r =
           CCResult.get_exn
           @@ Timedesc.Timestamp.of_iso8601
           @@ Timedesc.Timestamp.to_rfc3339 ~frac_s timestamp
         in
         Timedesc.Span.(
           abs (r - timestamp)
           < make ~s:0L
             ~ns:(int_of_float (10. ** float_of_int (CCInt.sub 9 frac_s)))
             ()))

  let iso_week_date_time_to_iso8601_nano_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"iso_week_date_time_to_iso8601_nano_of_iso8601_is_lossless" date_time (fun dt ->
          let ymd = Timedesc.ymd_date dt in
          QCheck.assume (ymd.year > 0 || (ymd.year =0 && ymd.day >= 3));
          let r =
            CCResult.get_exn
            @@ Timedesc.ISO_week_date_time.of_iso8601
            @@ Timedesc.ISO_week_date_time.to_iso8601 ~frac_s:9 dt
          in
          Timedesc.(Span.equal (to_timestamp_single r) (to_timestamp_single dt))
        )

  let iso_week_date_time_to_iso8601_w_default_frac_s_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"iso_week_date_time_to_iso8601_w_default_frac_s_of_iso8601_is_lossless" date_time
      (fun dt ->
         let ymd = Timedesc.ymd_date dt in
         QCheck.assume (ymd.year > 0 || (ymd.year =0 && ymd.day >= 3));
         let r =
           CCResult.get_exn
           @@ Timedesc.ISO_week_date_time.of_iso8601
           @@ Timedesc.ISO_week_date_time.to_iso8601 dt
         in
         Timedesc.(Span.equal (to_timestamp_single r) (to_timestamp_single dt))
      )

  let iso_week_date_time_to_iso8601_of_iso8601_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"iso_week_date_time_to_iso8601_of_iso8601_is_accurate"
      QCheck.(pair (int_bound 9) date_time)
      (fun (frac_s, dt) ->
         let ymd = Timedesc.ymd_date dt in
         QCheck.assume (ymd.year > 0 || (ymd.year =0 && ymd.day >= 3));
         let r =
           CCResult.get_exn
           @@ Timedesc.ISO_week_date_time.of_iso8601
           @@ Timedesc.ISO_week_date_time.to_iso8601 ~frac_s dt
         in
         let r = Timedesc.to_timestamp_single r in
         let timestamp = Timedesc.to_timestamp_single dt in
         Timedesc.Span.(
           abs (r - timestamp)
           < make ~s:0L
             ~ns:(int_of_float (10. ** float_of_int (CCInt.sub 9 frac_s)))
             ()))

  let iso_ord_date_time_to_iso8601_nano_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"iso_ord_date_time_to_iso8601_nano_of_iso8601_is_lossless" date_time (fun dt ->
          let r =
            CCResult.get_exn
            @@ Timedesc.ISO_ord_date_time.of_iso8601
            @@ Timedesc.ISO_ord_date_time.to_iso8601 ~frac_s:9 dt
          in
          Timedesc.(Span.equal (to_timestamp_single r) (to_timestamp_single dt))
        )

  let iso_ord_date_time_to_iso8601_w_default_frac_s_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"iso_ord_date_time_to_iso8601_w_default_frac_s_of_iso8601_is_lossless" date_time
      (fun dt ->
         let r =
           CCResult.get_exn
           @@ Timedesc.ISO_ord_date_time.of_iso8601
           @@ Timedesc.ISO_ord_date_time.to_iso8601 dt
         in
         Timedesc.(Span.equal (to_timestamp_single r) (to_timestamp_single dt))
      )

  let iso_ord_date_time_to_iso8601_of_iso8601_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"iso_ord_date_time_to_iso8601_of_iso8601_is_accurate"
      QCheck.(pair (int_bound 9) date_time)
      (fun (frac_s, dt) ->
         let r =
           CCResult.get_exn
           @@ Timedesc.ISO_ord_date_time.of_iso8601
           @@ Timedesc.ISO_ord_date_time.to_iso8601 ~frac_s dt
         in
         let r = Timedesc.to_timestamp_single r in
         let timestamp = Timedesc.to_timestamp_single dt in
         Timedesc.Span.(
           abs (r - timestamp)
           < make ~s:0L
             ~ns:(int_of_float (10. ** float_of_int (CCInt.sub 9 frac_s)))
             ()))

  let to_rfc9110_of_rfc9110_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"to_rfc9110_of_rfc9110_is_accurate"
      date_time
      (fun dt ->
         let r =
           CCResult.get_exn
           @@ Timedesc.of_rfc9110
           @@ Timedesc.to_rfc9110 dt
         in
         let r = Timedesc.to_timestamp_single r in
         let timestamp = Timedesc.to_timestamp_single dt in
         Timedesc.Span.(
           abs (r - timestamp)
           < make ~s:1L ()))

  let timestamp_to_rfc9110_of_rfc9110_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"timestamp_to_rfc9110_of_rfc9110_is_accurate"
      timestamp
      (fun timestamp ->
         let r =
           CCResult.get_exn
           @@ Timedesc.Timestamp.of_rfc9110
           @@ Timedesc.Timestamp.to_rfc9110 timestamp
         in
         Timedesc.Span.(
           abs (r - timestamp)
           < make ~s:1L ()))

  let of_to_timestamp =
    QCheck.Test.make ~count:100_000 ~name:"of_to_timestamp"
      QCheck.(pair time_zone timestamp)
      (fun (tz, timestamp) ->
         let r =
           Timedesc.to_timestamp_single
           @@ CCOption.get_exn_or "Expected successful construction of date time"
           @@ Timedesc.of_timestamp ~tz_of_date_time:tz timestamp
         in
         Timedesc.Span.equal r timestamp)

  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" date_time (fun s ->
        let s' =
          s |> Timedesc_sexp.to_sexp |> Timedesc_sexp.of_sexp |> CCResult.get_exn
        in
        Timedesc.equal s s')

  let zoneless_to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"zoneless_to_of_sexp" zoneless
      (fun s ->
         let s' =
           s
           |> Timedesc_sexp.Zoneless.to_sexp
           |> Timedesc_sexp.Zoneless.of_sexp
           |> CCResult.get_exn
         in
         Timedesc.Zoneless.equal s s')

  let timestamp_to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"timestamp_to_of_sexp" timestamp
      (fun s ->
         let s' =
           s
           |> Timedesc_sexp.Timestamp.to_sexp
           |> Timedesc_sexp.Timestamp.of_sexp
           |> CCResult.get_exn
         in
         Timedesc.Timestamp.equal s s')

  let consistent_with_ptime =
    QCheck.Test.make ~count:100_000 ~name:"consistent_with_ptime"
      QCheck.(pair ymd_date time)
      (fun ((year, month, day), (hour, minute, second, _ns)) ->
         let ptime =
           CCOption.get_exn_or "Expected successful ptime construction"
           @@ Ptime.of_date_time ((year, month, day), ((hour, minute, second), 0))
         in
         let dt =
           Timedesc.make_unambiguous_exn ~year ~month ~day ~hour ~minute ~second
             ~offset_from_utc:Timedesc.Span.zero ()
         in
         Timedesc.Utils.timestamp_of_ptime ptime
         = Timedesc.to_timestamp_single dt)

  let iso_ord_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"iso_ord_date_accessors"
      QCheck.(pair iso_ord_date time)
      (fun ((year', day_of_year'), (hour, minute, second, ns)) ->
         let d =
           Timedesc.ISO_ord_date_time.make_exn ~tz:Timedesc.Time_zone.utc
             ~year:year' ~day_of_year:day_of_year' ~hour ~minute ~second ~ns ()
         in
         let year = Timedesc.year d in
         let day_of_year = Timedesc.day_of_year d in
         year = year' && day_of_year = day_of_year')

  let iso_week_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"iso_week_date_accessors"
      QCheck.(pair iso_week_date time)
      (fun ((year', week', weekday'), (hour, minute, second, ns)) ->
         let d =
           Timedesc.ISO_week_date_time.make_exn ~tz:Timedesc.Time_zone.utc
             ~year:year' ~week:week' ~weekday:weekday' ~hour ~minute ~second ~ns
             ()
         in
         let year = Timedesc.iso_year d in
         let year'', week = Timedesc.ISO_week.year_week @@ Timedesc.iso_week d in
         let weekday = Timedesc.weekday d in
         year = year' && year = year'' && week = week' && weekday = weekday')

  let ymd_date_accessors =
    QCheck.Test.make ~count:100_000 ~name:"ymd_date_accessors"
      QCheck.(pair ymd_date time)
      (fun ((year', month', day'), (hour, minute, second, ns)) ->
         let d =
           Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:year' ~month:month'
             ~day:day' ~hour ~minute ~second ~ns ()
         in
         let year = Timedesc.year d in
         let month = Timedesc.month d in
         let day = Timedesc.day d in
         year = year' && month = month' && day = day')

  let time_accessors =
    QCheck.Test.make ~count:100_000 ~name:"time_accessors"
      QCheck.(pair ymd_date time)
      (fun ((year', month', day'), (hour, minute, second, ns)) ->
         let d =
           Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year:year' ~month:month'
             ~day:day' ~hour ~minute ~second ~ns ()
         in
         let hour' = Timedesc.hour d in
         let minute' = Timedesc.minute d in
         let second' = Timedesc.second d in
         let ns' = Timedesc.ns d in
         hour = hour' && minute = minute' && second = second' && ns = ns')

  let suite =
    [
      to_rfc3339_nano_of_iso8601_is_lossless;
      to_rfc3339_w_default_frac_s_of_iso8601_is_lossless;
      to_rfc3339_of_iso8601_is_accurate;
      timestamp_to_rfc3339_nano_of_iso8601_is_lossless;
      timestamp_to_rfc3339_w_default_frac_s_of_iso8601_is_lossless;
      timestamp_to_rfc3339_of_iso8601_is_accurate;
      iso_week_date_time_to_iso8601_nano_of_iso8601_is_lossless;
      iso_week_date_time_to_iso8601_w_default_frac_s_of_iso8601_is_lossless;
      iso_week_date_time_to_iso8601_of_iso8601_is_accurate;
      iso_ord_date_time_to_iso8601_nano_of_iso8601_is_lossless;
      iso_ord_date_time_to_iso8601_w_default_frac_s_of_iso8601_is_lossless;
      iso_ord_date_time_to_iso8601_of_iso8601_is_accurate;
      to_rfc9110_of_rfc9110_is_accurate;
      timestamp_to_rfc9110_of_rfc9110_is_accurate;
      of_to_timestamp;
      to_of_sexp;
      zoneless_to_of_sexp;
      timestamp_to_of_sexp;
      consistent_with_ptime;
      iso_ord_date_accessors;
      iso_week_date_accessors;
      ymd_date_accessors;
      time_accessors;
    ]
end
