open Test_utils

module Alco = struct
  let leap_second1 () =
    Alcotest.(check span_testable)
      "same timestamp"
      (Time.Date_time'.make_exn ~tz:Time_zone.utc ~year:2020 ~month:`Jan ~day:1 ~hour:0 ~minute:0 ~second:60 ~ns:1_000_000 ()
       |> Time.Date_time'.to_timestamp_single
      )
      (Time.Date_time'.make_exn ~tz:Time_zone.utc ~year:2020 ~month:`Jan ~day:1 ~hour:0 ~minute:0 ~second:59 ~ns:1_000_000 ()
       |> Time.Date_time'.to_timestamp_single
      )

  let suite =
    [Alcotest.test_case "leap_second1" `Quick leap_second1]
end

module Qc = struct
  let to_rfc3339_nano_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"to_rfc3339_nano_of_iso8601_is_lossless" timestamp (fun timestamp ->
          let r =
            CCResult.get_exn
            @@ ISO8601.to_timestamp
            @@ RFC3339.of_timestamp ~frac_s:9 timestamp
          in
          Span.equal r timestamp)

  let to_rfc3339_w_default_frac_s_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"to_rfc3339_w_default_frac_s_of_iso8601_is_lossless" timestamp (fun timestamp ->
          let r =
            CCResult.get_exn
            @@ ISO8601.to_timestamp
            @@ RFC3339.of_timestamp timestamp
          in
          Span.equal r timestamp)

  let to_rfc3339_of_iso8601_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"to_rfc3339_of_iso8601_is_accurate"
      QCheck.(pair (int_bound 9) timestamp)
      (fun (frac_s, timestamp) ->
         let r =
           CCResult.get_exn
           @@ ISO8601.to_timestamp
           @@ RFC3339.of_timestamp ~frac_s timestamp
         in
         Span.(
           abs (r - timestamp)
           < make ~s:0L
             ~ns:(int_of_float (10. ** float_of_int (CCInt.sub 9 frac_s)))
             ()))

  let of_to_timestamp =
    QCheck.Test.make ~count:100_000 ~name:"of_to_timestamp"
      QCheck.(pair time_zone timestamp)
      (fun (tz, timestamp) ->
         let r =
           Time.Date_time'.to_timestamp_single
           @@ CCOpt.get_exn
           @@ Time.Date_time'.of_timestamp ~tz_of_date_time:tz timestamp
         in
         Span.equal r timestamp)

  let suite =
    [
      to_rfc3339_nano_of_iso8601_is_lossless;
      to_rfc3339_w_default_frac_s_of_iso8601_is_lossless;
      to_rfc3339_of_iso8601_is_accurate;
      of_to_timestamp;
    ]
end
