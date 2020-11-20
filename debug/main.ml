let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {wday:Xxx} {hour:0X}:{min:0X}:{sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {swday:Xxx} {shour:0X}:{smin:0X}:{ssec:0X}, \
   {eyear} {emon:Xxx} {emday:0X} {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X})"

let debug_branching () =
  Timere.branching ~months:[`Range_inc (`Nov, `Oct)] ()
  |> ignore

let debug_resolve () =
  let expr =
    "nov to oct"
  in
  let search_years_ahead = 5 in
  match Timere_parse.timere expr with
  | Error msg -> print_endline msg
  | Ok timere -> (
      let cur_date_time =
        Result.get_ok
        @@ Timere.Date_time.cur ()
      in
      let search_start_timere =
        Result.get_ok @@ Timere.of_date_time cur_date_time
      in
      let search_end_exc_timere =
        let open Timere in
        shift
          (Result.get_ok @@ Duration.make ~days:(search_years_ahead * 365) ())
          search_start_timere
      in
      let timere =
        let open Timere in
        inter timere (interval_exc search_start_timere search_end_exc_timere)
      in
      match Timere.resolve timere with
      | Error msg -> print_endline msg
      | Ok s ->
        match s () with
        | Seq.Nil -> print_endline "No matching time slots"
        | Seq.Cons _ ->
          s
          |> OSeq.take 100
          |> OSeq.iter (fun ts ->
              match
                Timere.sprintf_interval
                  default_interval_format_string ts
              with
              | Ok s ->
                Printf.printf "%s\n" s
              | Error msg -> Printf.printf "Error: %s\n" msg);
          print_newline () )

let () =
  debug_branching ()

(* let () =
 *   debug_resolve () *)
