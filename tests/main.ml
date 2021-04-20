let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {wday:Xxx} {hour:0X}:{min:0X}:{sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {swday:Xxx} {shour:0X}:{smin:0X}:{ssec:0X}, \
   {eyear} {emon:Xxx} {emday:0X} {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X})"

(* let () =
 *   let search_years_ahead = 1 in
 *   let cur_date_time = CCResult.get_ok @@ Time.Date_time.cur () in
 *   let search_start = Time.Date_time.to_timestamp cur_date_time in
 *   let search_end_exc =
 *     Time.Date_time.make
 *       ~year:(cur_date_time.year + search_years_ahead)
 *       ~month:cur_date_time.month ~day:cur_date_time.day ~hour:cur_date_time.hour
 *       ~minute:cur_date_time.minute ~second:cur_date_time.second
 *       ~tz_offset_s:cur_date_time.tz_offset_s
 *     |> CCResult.get_ok
 *     |> Time.Date_time.to_timestamp
 *   in
 *   let timere = Time.pattern ~months:[ `Jan ] () in
 *   let s =
 *     Resolver_simple.resolve ~search_start ~search_end_exc ~tz_offset_s:0 timere
 *   in
 *   match s () with
 *   | Seq.Nil -> print_endline "No matching time slots"
 *   | Seq.Cons _ ->
 *     s
 *     |> OSeq.take 20
 *     |> OSeq.iter (fun ts ->
 *         match
 *           Printer.sprintf_interval default_interval_format_string ts
 *         with
 *         | Ok s -> Printf.printf "%s\n" s
 *         | Error msg -> Printf.printf "Error: %s\n" msg);
 *     print_newline () *)

let () =
  let alco_suites =
    [
      ("Date_time_tests.Alco", Date_time_tests.Alco.suite);
      ("Time_intervals_tests.Alco", Time_intervals_tests.Alco.suite);
      ("Time_tests.Alco", Time_tests.Alco.suite);
      ("Tzdb_tests.Alco", Tzdb_tests.Alco.suite);
    ]
  in
  let qc_suites =
    [
      ("Span_tests.Qc", Span_tests.Qc.suite);
      ("Duration_tests.Qc", Duration_tests.Qc.suite);
      ("Date_time_tests.Qc", Date_time_tests.Qc.suite);
      ("Time_intervals_tests.Qc", Time_intervals_tests.Qc.suite);
      ("Time_tests.Qc", Time_tests.Qc.suite);
      ("Resolver_tests.Qc", Resolver_tests.Qc.suite);
      ("Time_zone_tests.Qc", Time_zone_tests.Qc.suite);
    ]
    |> List.map (fun (name, suite) ->
        (name, List.map QCheck_alcotest.to_alcotest suite))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "timere" suites
