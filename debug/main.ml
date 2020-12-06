let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {wday:Xxx} {hour:0X}:{min:0X}:{sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {swday:Xxx} {shour:0X}:{smin:0X}:{ssec:0X}, \
   {eyear} {emon:Xxx} {emday:0X} {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X})"

let debug_branching () =
  let timere =
    Timere.branching
      ~months:[ `Range_inc (`Jan, `Mar) ]
      ~days:(Timere.Month_days [ `Range_inc (-2, -1) ])
      ()
  in
  let search_years_ahead = 5 in
  let cur_date_time = Result.get_ok @@ Timere.Date_time.cur () in
  let search_end_exc =
    Result.get_ok
    @@ Timere.Date_time.make
      ~year:(cur_date_time.year + search_years_ahead)
      ~month:cur_date_time.month ~day:cur_date_time.day
      ~hour:cur_date_time.hour ~minute:cur_date_time.minute
      ~second:cur_date_time.second ~tz_offset_s:cur_date_time.tz_offset_s
  in
  let timere =
    Timere.(inter [ timere; interval_dt_exc cur_date_time search_end_exc ])
  in
  match Timere.resolve timere with
  | Error msg -> print_endline msg
  | Ok s -> (
      match s () with
      | Seq.Nil -> print_endline "No matching time slots"
      | Seq.Cons _ ->
        s
        |> OSeq.take 20
        |> OSeq.iter (fun ts ->
            match
              Timere.sprintf_interval default_interval_format_string ts
            with
            | Ok s -> Printf.printf "%s\n" s
            | Error msg -> Printf.printf "Error: %s\n" msg);
        print_newline () )

let debug_parsing () =
  let expr = "dec to jan" in
  let search_years_ahead = 5 in
  match Timere_parse.timere expr with
  | Error msg -> print_endline msg
  | Ok timere -> (
      let cur_date_time = Result.get_ok @@ Timere.Date_time.cur () in
      let search_end_exc =
        Result.get_ok
        @@ Timere.Date_time.make
          ~year:(cur_date_time.year + search_years_ahead)
          ~month:cur_date_time.month ~day:cur_date_time.day
          ~hour:cur_date_time.hour ~minute:cur_date_time.minute
          ~second:cur_date_time.second ~tz_offset_s:cur_date_time.tz_offset_s
      in
      let timere =
        Timere.(inter [ timere; interval_dt_exc cur_date_time search_end_exc ])
      in
      match Timere.resolve timere with
      | Error msg -> print_endline msg
      | Ok s -> (
          match s () with
          | Seq.Nil -> print_endline "No matching time slots"
          | Seq.Cons _ ->
            s
            |> OSeq.take 100
            |> OSeq.iter (fun ts ->
                match
                  Timere.sprintf_interval default_interval_format_string ts
                with
                | Ok s -> Printf.printf "%s\n" s
                | Error msg -> Printf.printf "Error: %s\n" msg);
            print_newline () ) )

let debug_resolver () =
  let s =
    {|
(chunk 918
(intervals ((2002 Jan 14 21 5 18 (tz_offset_s 0)) (2002 Jan 14 21 7 30 (tz_offset_s 0))) ((2002 May 24 5 18 12 (tz_offset_s 0)) (2002 May 24 5 23 4 (tz_offset_s 0))) ((2002 Jun 19 12 52 33 (tz_offset_s 0)) (2002 Jun 19 12 52 38 (tz_offset_s 0))) ((2002 Jul 9 4 33 5 (tz_offset_s 0)) (2002 Jul 9 4 48 23 (tz_offset_s 0))) ((2002 Oct 6 6 12 52 (tz_offset_s 0)) (2002 Oct 6 6 18 25 (tz_offset_s 0))))
)
|}
  in
  let timere = Result.get_ok @@ Timere.of_sexp_string s in
  (* let timere =
   *   (fun height max_branching randomness ->
   *      Timere.Utils.build ~min_year:2000 ~max_year_inc:2002 ~height ~max_branching
   *        ~randomness)
   *     2 5 [333; 5; 918; 132; 292]
   * in *)
  print_endline (Timere.to_sexp_string timere);
  print_endline "=====";
  let search_start_dt =
    Result.get_ok
    @@ Timere.Date_time.make ~year:2000 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz_offset_s:0
  in
  let search_start = Timere.Date_time.to_timestamp search_start_dt in
  let search_end_exc_dt =
    Result.get_ok
    @@ Timere.Date_time.make ~year:2003 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz_offset_s:0
  in
  let search_end_exc = Timere.Date_time.to_timestamp search_end_exc_dt in
  ( match
      Timere.resolve
        Timere.(inter [ timere; interval_exc search_start search_end_exc ])
    with
    | Error msg -> print_endline msg
    | Ok s -> (
        match s () with
        | Seq.Nil -> print_endline "No matching time slots"
        | Seq.Cons _ ->
          s
          |> OSeq.take 50
          |> OSeq.iter (fun ts ->
              match
                Timere.sprintf_interval ~display_using_tz_offset_s:0
                  default_interval_format_string ts
              with
              | Ok s -> Printf.printf "%s\n" s
              | Error msg -> Printf.printf "Error: %s\n" msg) ) );
  print_endline "=====";
  let s =
    Timere.Utils.resolve_simple ~search_start ~search_end_exc
      ~search_using_tz_offset_s:0 timere
  in
  ( match s () with
    | Seq.Nil -> print_endline "No matching time slots"
    | Seq.Cons _ ->
      s
      |> OSeq.take 50
      |> OSeq.iter (fun ts ->
          match
            Timere.sprintf_interval ~display_using_tz_offset_s:0
              default_interval_format_string ts
          with
          | Ok s -> Printf.printf "%s\n" s
          | Error msg -> Printf.printf "Error: %s\n" msg) );
  print_newline ()

(* let () = debug_branching () *)

(* let () =
 *   debug_parsing () *)

let () = debug_resolver ()
