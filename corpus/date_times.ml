let texts = [
  "2020 jun 6 10am";
  "2020 jun 6th 10am";
  "6 jun 2020 10am";
  "6th jun 2020 10am";
  "2020 jun 6 10:15";
  "2020 jun 6th 10:15";
  "6 jun 2020 10:15";
  "6th jun 2020 10:15";
  "Australia/Sydney 2020 jun 6 10am";
  "Australia/Sydney 2020 jun 6 10:15";
  "2020 jun 6 10am Australia/Sydney";
  "2020 jun 6 10:15 Australia/Sydney";
  "2020-06-01 10:10";
  "2020/06/01 10am";
  "10:10 2020-06-01";
  "10am 2020/06/01";
  "01-06-2020 10:10";
  "01/06/2020 10am";
  "10:10 01-06-2020";
  "10am 01/06/2020";
  "6th of jul 2021 9:15am";
  "6th of jul 2021 9:51";
  "2021 6 of jul 9:15am";
  "2021 6th of jul 9:51";
  "jul 6 2021 9:15am";
  "jul 6th 2021 9:15am";
  "2020 jul 6";
  "2020 jul 6th";
  "6 jul 2020";
  "6th jul 2020";
  "2020/06/01";
  "01/06/2020";
  "1st of jun 2020";
]

let () =
  List.iteri (fun i text ->
      Printf.printf "%d. %S\n" i text;
      match Timere_parse.date_time ~tz:Timere.Time_zone.utc text with
      | Ok dt ->
        Printf.printf "  Ok %s\n\n" (Timere.Date_time.to_rfc3339 dt)
      | Error msg ->
        Printf.printf "  Error %s\n" msg;
        print_endline "  ^^^^^";
        print_newline ();
    ) texts
