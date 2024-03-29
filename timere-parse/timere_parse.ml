exception Error_exn of string

let timere = Parser.parse_timere

let timere_exn x =
  match timere x with Ok x -> x | Error msg -> raise (Error_exn msg)

let date_time = Parser.parse_date_time

let date_time_exn ?tz x =
  match date_time ?tz x with Ok x -> x | Error msg -> raise (Error_exn msg)

let hms = Parser.parse_hms

let hms_exn x =
  match hms x with Ok x -> x | Error msg -> raise (Error_exn msg)

let span = Parser.parse_span

let span_exn x =
  match span x with Ok x -> x | Error msg -> raise (Error_exn msg)
