let texts =
  [
    "24h";
    "16.5 hours";
    "1h20min";
    "1 hour 2.5 minutes";
    "0 seconds";
    "10 seconds";
    "100 seconds";
    "2.25 minutes 1 seconds";
    "5 days 6.5 hours";
    "0.1 hours";
    "0.01 hours";
  ]

let () =
  List.iteri
    (fun i text ->
       Printf.printf "%d. %S\n" i text;
       match Timere_parse.duration text with
       | Ok duration ->
         Printf.printf "  Ok %s\n\n" (Timere.Duration.to_string duration)
       | Error msg ->
         Printf.printf "  Error %s\n" msg;
         print_endline "  ^^^^^";
         print_newline ())
    texts
