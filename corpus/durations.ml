let texts = [
  "24h";
  "16.5 hours";
  "1h20min";
]

let () =
  List.iteri (fun i text ->
      Printf.printf "%d. %S\n" i text;
      match Timere_parse.duration text with
      | Ok duration ->
        Printf.printf "  Ok %s\n\n" (Timere.Duration.to_string duration)
      | Error msg ->
        Printf.printf "  Error %s\n" msg;
        print_endline "  ^^^^^";
        print_newline ();
    ) texts
