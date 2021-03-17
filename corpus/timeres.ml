let texts = [
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
  "10am to 4pm";
]

let () =
  List.iteri (fun i text ->
      Printf.printf "%d. %S\n" i text;
      match Timere_parse.timere text with
      | Ok timere ->
        Fmt.pr "  Ok %a\n\n" Timere.pp_sexp timere
      | Error msg ->
        Printf.printf "  Error %s\n" msg;
        print_endline "  ^^^^^";
        print_newline ();
    ) texts
