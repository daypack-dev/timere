let debug_parsing () =
  let expr = "(mar 2 1pm to 4th 14:00 || jun) && thursday" in
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
  let dura = "7d" in
  match Timere_parse.duration dura with
  | Error msg -> print_endline msg
  | Ok timere ->
    Fmt.pr "%a@."
      Timere.Duration.pp
      timere

(* let () =
 *   debug_parsing () *)

let () =
  debug_duration ()
