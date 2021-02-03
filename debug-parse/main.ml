let debug_parsing () =
  let expr = "jun 1 00:00 to feb 1 00:00" in
  print_endline expr;
  print_newline ();
  print_endline "gives";
  print_newline ();
  match Timere_parse.timere expr with
  | Error msg -> print_endline msg
  | Ok timere ->
    Fmt.pr "%a@."
    Timere.pp_sexp
    timere

let () =
  debug_parsing ()
