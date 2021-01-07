let debug_parsing () =
  let expr = "30 th, 1, 2, 29" in
  match Timere_parse.timere expr with
  | Error msg -> print_endline msg
  | Ok timere -> print_endline (Timere.to_sexp_string timere)

let () =
  debug_parsing ()
