let texts = [
  "2020 jun";
  "jan";
  "jan 6th";
  "jan 6";
  "jan 6 00:00";
  "jan 6 12pm to 2pm";
  "12pm to 2pm";
  "12th, 13th";
]

let () =
  List.iteri (fun i text ->
      Printf.printf "%d. %S\n" i text;
      match Timere_parse.timere text with
      | Ok timere ->
        Printf.printf "  Ok %s\n\n" (Timere.to_sexp_string timere)
      | Error msg ->
        Printf.printf "  Error %s\n" msg;
        print_endline "  ^^^^^";
        print_newline ();
    ) texts
