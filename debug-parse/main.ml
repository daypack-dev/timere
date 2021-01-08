let debug_parsing () =
  let expr = "2020 jan 01 09:30 pm" in
  match Timere_parse.timere expr with
  | Error msg -> print_endline msg
  | Ok timere -> print_endline (Timere.to_sexp_string timere)

let () =
  debug_parsing ()
