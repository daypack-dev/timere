let debug_parsing () =
  let expr = "16th 6am to 8am, 9am" in
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

let debug_duration () =
  let dur = "7d" in
  match Timere_parse.duration dur with
  | Error msg -> print_endline msg
  | Ok dur ->
    Fmt.pr "%a@."
      Timere.Duration.pp
      dur

let () =
  debug_parsing ()

(* let () =
 *   debug_duration () *)
