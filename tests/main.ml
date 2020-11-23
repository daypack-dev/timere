let () =
  let alco_suites =
    [
      ("Time_slots.Alco", Time_intervals_tests.Alco.suite);
    ]
  in
  let qc_suites =
    [
      ("Time_slots.Qc", Time_intervals_tests.Qc.suite);
    ]
    |> List.map (fun (name, suite) ->
        (name, List.map QCheck_alcotest.to_alcotest suite))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "timere" suites

