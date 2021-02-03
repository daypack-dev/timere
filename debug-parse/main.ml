let debug_parsing () =
  let expr = "2020 jan 1 00:00 to 2020 feb 1 00:00" in
  match Timere_parse.timere expr with
  | Error msg -> print_endline msg
  | Ok timere -> print_endline (Timere.to_sexp_string timere)

let () =
  debug_parsing ()
