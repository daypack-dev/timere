let texts =
  [
    "10am";
    "10:15";
    "20:30";
    "23:59:59";
    "10:59:59am";
    "10:59:59 pm";
    "24:00:00";
  ]

let () =
  List.iteri
    (fun i text ->
      Printf.printf "%d. %S\n" i text;
      match Timere_parse.hms text with
      | Ok hms ->
          let { Timedesc.Time.hour; minute; second; _ } =
            Timedesc.Time.view hms
          in
          Printf.printf "  Ok %d:%d:%d\n\n%!" hour minute second
      | Error msg ->
          Printf.printf "  Error %s\n" msg;
          print_endline "  ^^^^^";
          print_newline ())
    texts
