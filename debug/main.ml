let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {wday:Xxx} {hour:0X}:{min:0X}:{sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {swday:Xxx} {shour:0X}:{smin:0X}:{ssec:0X}, \
   {eyear} {emon:Xxx} {emday:0X} {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X})"

let debug_branching () =
  let timere =
    Timere.branching ~months:[`Range_inc (`Dec, `Dec)]
      ~days:(Timere.Month_days [`Range_inc (1, 10); `Range_inc (11, 27)])
      ()
  in
  let search_years_ahead = 5 in
  let cur_date_time =
    Result.get_ok
    @@ Timere.Date_time.cur ()
  in
  let search_end_exc =
    Result.get_ok @@
    Timere.Date_time.make ~year:(cur_date_time.year + search_years_ahead)
      ~month:cur_date_time.month
      ~day:cur_date_time.day
      ~hour:cur_date_time.hour
      ~minute:cur_date_time.minute
      ~second:cur_date_time.second
      ~tz_offset_s:cur_date_time.tz_offset_s
  in
  let timere =
    let open Timere in
    inter timere (interval_exc cur_date_time search_end_exc)
  in
  match Timere.resolve timere with
  | Error msg -> print_endline msg
  | Ok s ->
    match s () with
    | Seq.Nil -> print_endline "No matching time slots"
    | Seq.Cons _ ->
      s
      |> OSeq.take 10
      |> OSeq.iter (fun ts ->
          match
            Timere.sprintf_interval
              default_interval_format_string ts
          with
          | Ok s ->
            Printf.printf "%s\n" s
          | Error msg -> Printf.printf "Error: %s\n" msg);
      print_newline ()

let debug_parsing () =
  let expr =
    "nov to dec"
  in
  let search_years_ahead = 5 in
  match Timere_parse.timere expr with
  | Error msg -> print_endline msg
  | Ok timere -> (
      let cur_date_time =
        Result.get_ok
        @@ Timere.Date_time.cur ()
      in
      let search_end_exc =
        Result.get_ok @@
        Timere.Date_time.make ~year:(cur_date_time.year + search_years_ahead)
          ~month:cur_date_time.month
          ~day:cur_date_time.day
          ~hour:cur_date_time.hour
          ~minute:cur_date_time.minute
          ~second:cur_date_time.second
          ~tz_offset_s:cur_date_time.tz_offset_s
      in
      let timere =
        let open Timere in
        inter timere (interval_exc cur_date_time search_end_exc)
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
 *   debug_parsing () *)
