let texts = [
  "24h";
  "1h20min";
]

let () =
  List.iteri (fun i text ->
      Printf.printf "%d. %S\n" i text;
      match Timere_parse.duration text with
      | Ok duration ->
        Fmt.pr "  Ok %a\n\n" Timere.Duration.pp_sexp duration
      | Error msg ->
        Printf.printf "  Error %s\n" msg;
        print_endline "  ^^^^^";
        print_newline ();
    ) texts
