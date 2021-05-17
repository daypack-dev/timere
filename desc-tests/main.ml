let () =
  let alco_suites =
    [
      ("Time_tests.Alco", Time_tests.Alco.suite);
      ("Date_time_tests.Alco", Date_time_tests.Alco.suite);
      ("Tzdb_tests.Alco", Tzdb_tests.Alco.suite);
    ]
  in
  let qc_suites =
    [
      ("Span_tests.Qc", Span_tests.Qc.suite);
      ("Date_tests.Qc", Date_tests.Qc.suite);
      ("Date_time_tests.Qc", Date_time_tests.Qc.suite);
      ("Ptime_tests.Qc", Ptime_tests.Qc.suite);
      ("Time_zone_tests.Qc", Time_zone_tests.Qc.suite);
    ]
    |> List.map (fun (name, suite) ->
        (name, List.map QCheck_alcotest.to_alcotest suite))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "timedesc" suites
