open Test_utils

module Alco = struct
  let of_iso8601_case0 () =
    Alcotest.(check time_testable)
      "same date"
      (Timedesc.Time.of_iso8601_exn "12:34")
      (Timedesc.Time.make_exn ~hour:12 ~minute:34 ~second:0 ())

  let of_iso8601_case1 () =
    Alcotest.(check time_testable)
      "same date"
      (Timedesc.Time.of_iso8601_exn "12:34:21")
      (Timedesc.Time.make_exn ~hour:12 ~minute:34 ~second:21 ())

  let of_iso8601_case2 () =
    Alcotest.(check time_testable)
      "same date"
      (Timedesc.Time.of_iso8601_exn "12:34:21.0001")
      (Timedesc.Time.make_exn ~hour:12 ~minute:34 ~second:21 ~ns:000_100_000 ())

  let of_iso8601_case3 () =
    Alcotest.(check time_testable)
      "same date"
      (Timedesc.Time.of_iso8601_exn "12:34:21.00010002")
      (Timedesc.Time.make_exn ~hour:12 ~minute:34 ~second:21 ~ns:000_100_020 ())

  let end_of_day_24_00_00 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Time.make_exn ~hour:24 ~minute:0 ~second:0 ~ns:0 ()
       |> Timedesc.Time.to_span)
      (Timedesc.Time.make_exn ~hour:23 ~minute:59 ~second:59 ~ns:999_999_999 ()
       |> Timedesc.Time.to_span)

  let suite =
    [
      Alcotest.test_case "of_iso8601_case0" `Quick of_iso8601_case0;
      Alcotest.test_case "of_iso8601_case1" `Quick of_iso8601_case1;
      Alcotest.test_case "of_iso8601_case2" `Quick of_iso8601_case2;
      Alcotest.test_case "of_iso8601_case3" `Quick of_iso8601_case3;
      Alcotest.test_case "end_of_day_24_00_00" `Quick end_of_day_24_00_00;
    ]
end

module Qc = struct
  let to_rfc3339_nano_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"to_rfc3339_nano_of_iso8601_is_lossless" time
      (fun (hour, minute, second, ns) ->
         let t = Timedesc.Time.make_exn ~hour ~minute ~second ~ns () in
         let r =
           CCResult.get_exn
           @@ Timedesc.Time.of_iso8601
           @@ Timedesc.Time.to_rfc3339 ~frac_s:9 t
         in
         Timedesc.Time.equal r t)

  let to_rfc3339_w_default_frac_s_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"to_rfc3339_w_default_frac_s_of_iso8601_is_lossless" time
      (fun (hour, minute, second, ns) ->
         let t = Timedesc.Time.make_exn ~hour ~minute ~second ~ns () in
         let r =
           CCResult.get_exn
           @@ Timedesc.Time.of_iso8601
           @@ Timedesc.Time.to_rfc3339 t
         in
         Timedesc.Time.equal r t)

  let to_rfc3339_of_iso8601_is_accurate =
    QCheck.Test.make ~count:100_000 ~name:"to_rfc3339_of_iso8601_is_accurate"
      QCheck.(pair (int_bound 9) time)
      (fun (frac_s, (hour, minute, second, ns)) ->
         let t = Timedesc.Time.make_exn ~hour ~minute ~second ~ns () in
         let r =
           CCResult.get_exn
           @@ Timedesc.Time.of_iso8601
           @@ Timedesc.Time.to_rfc3339 ~frac_s t
         in
         let t_s = Timedesc.Time.to_span t in
         let r_s = Timedesc.Time.to_span r in
         Timedesc.Span.(
           abs (r_s - t_s)
           < make ~s:0L
             ~ns:(int_of_float (10. ** float_of_int (CCInt.sub 9 frac_s)))
             ()))

  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" time
      (fun (hour, minute, second, ns) ->
         let t = Timedesc.Time.make_exn ~hour ~minute ~second ~ns () in
         let t' =
           t
           |> Timedesc.Time.to_sexp
           |> Timedesc.Time.of_sexp
           |> CCResult.get_exn
         in
         Timedesc.Time.equal t t')

  let accessors =
    QCheck.Test.make ~count:100_000 ~name:"accessors" time
      (fun (hour, minute, second, ns) ->
         let time = Timedesc.Time.make_exn ~hour ~minute ~second ~ns () in
         let hour' = Timedesc.Time.hour time in
         let minute' = Timedesc.Time.minute time in
         let second' = Timedesc.Time.second time in
         let ns' = Timedesc.Time.ns time in
         hour = hour' && minute = minute' && second = second' && ns = ns')

  let suite =
    [
      to_rfc3339_nano_of_iso8601_is_lossless;
      to_rfc3339_w_default_frac_s_of_iso8601_is_lossless;
      to_rfc3339_of_iso8601_is_accurate;
      to_of_sexp;
      accessors;
    ]
end
