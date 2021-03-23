let texts = [
  "10am";
  "10:15";
  "20:30";
  "23:59:59";
]

let () =
  List.iteri (fun i text ->
      Printf.printf "%d. %S\n" i text;
      match Timere_parse.hms text with
      | Ok hms ->
        Printf.printf "  Ok %d:%d:%d\n\n" hms.hour hms.minute hms.second
      | Error msg ->
        Printf.printf "  Error %s\n" msg;
        print_endline "  ^^^^^";
        print_newline ();
    ) texts
