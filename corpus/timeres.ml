let texts =
  [
    "2020 jun";
    "jan";
    "jan 6th";
    "jan 6";
    "jan 6 00:00";
    "jan 6 12pm to 2pm";
    "12pm to 2pm";
    "jun 12, 13th";
    "12th, 13, 20";
    "12th, 13 to 15, 20";
    "10 to 12, 13th to 15, 20";
    "thursday";
    "fri";
    "tue";
    "16th 7:30am";
    "16th 7:30:01am";
    "16th 7:30:01pm";
    "16th 20:30:01";
    "16th 7:30am to 11am";
    "16th 11am to 6am";
    "16th 8am to 10am, 11am to 12pm";
    "16th 6am to 8am, 9am, 10am to 11am";
    "jan or Feb";
    "2020 jun 16th 10am to 12pm";
    "2020 jun 16th 10am to 17th 12pm";
    "2020 jun 16th 10am to jul 1 12pm";
    "2020.06.16";
    "2020.06.16 10am";
    "2021 aug 26th to 27th";
  ]

let () =
  List.iteri
    (fun i text ->
      Printf.printf "%d. %S\n" i text;
      match Timere_parse.timere text with
      | Ok timere -> Printf.printf "  Ok %s\n\n" (Timere.to_sexp_string timere)
      | Error msg ->
          Printf.printf "  Error %s\n" msg;
          print_endline "  ^^^^^";
          print_newline ())
    texts
