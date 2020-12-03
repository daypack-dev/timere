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
    Timere.(inter timere (interval_dt_exc cur_date_time search_end_exc))
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
        Timere.(inter timere (interval_dt_exc cur_date_time search_end_exc))
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
(round_robin (interval_inc (2075 Feb 11 3 7 39 (tz_offset_s 0)) (2075 Feb 11 3 9 5 (tz_offset_s 0))) (interval_exc (2073 Apr 20 22 51 7 (tz_offset_s 0)) (2073 Apr 20 22 52 46 (tz_offset_s 0))) (pattern (years 2007 2049 2051 2073 2075 2086 2094 2099) (months Feb Mar Apr Aug Nov) (month_days 2 7 8 12 14 19 21 25) (weekdays Sun Mon Tue Wed Fri) (hours 1 3 7 14 22) (minutes 7 13 15 26 34 39 49 51) (seconds 7 13 15 26 34 39 49 51)) (intervals ((2007 Apr 27 3 13 15 (tz_offset_s 0)) (2007 Apr 27 3 14 4 (tz_offset_s 0))) ((2049 Nov 22 7 39 26 (tz_offset_s 0)) (2049 Nov 22 7 41 5 (tz_offset_s 0))) ((2051 Aug 7 14 39 13 (tz_offset_s 0)) (2051 Aug 7 14 40 28 (tz_offset_s 0))) ((2073 Apr 20 22 51 7 (tz_offset_s 0)) (2073 Apr 20 22 52 46 (tz_offset_s 0))) ((2075 Feb 11 3 7 39 (tz_offset_s 0)) (2075 Feb 11 3 9 5 (tz_offset_s 0))) ((2086 Apr 14 3 49 34 (tz_offset_s 0)) (2086 Apr 14 3 50 25 (tz_offset_s 0))) ((2094 Apr 8 3 26 39 (tz_offset_s 0)) (2094 Apr 8 3 27 52 (tz_offset_s 0))) ((2099 Feb 20 1 34 51 (tz_offset_s 0)) (2099 Feb 20 1 34 58 (tz_offset_s 0))) ((2099 Mar 7 1 15 49 (tz_offset_s 0)) (2099 Mar 7 1 17 23 (tz_offset_s 0)))) (interval_exc (2094 Apr 8 3 26 39 (tz_offset_s 0)) (2094 Apr 8 3 27 52 (tz_offset_s 0))) (intervals ((2007 Apr 27 3 13 15 (tz_offset_s 0)) (2007 Apr 27 3 14 4 (tz_offset_s 0))) ((2049 Nov 22 7 39 26 (tz_offset_s 0)) (2049 Nov 22 7 41 5 (tz_offset_s 0))) ((2051 Aug 7 14 39 13 (tz_offset_s 0)) (2051 Aug 7 14 40 28 (tz_offset_s 0))) ((2073 Apr 20 22 51 7 (tz_offset_s 0)) (2073 Apr 20 22 52 46 (tz_offset_s 0))) ((2075 Feb 11 3 7 39 (tz_offset_s 0)) (2075 Feb 11 3 9 5 (tz_offset_s 0))) ((2086 Apr 14 3 49 34 (tz_offset_s 0)) (2086 Apr 14 3 50 25 (tz_offset_s 0))) ((2094 Apr 8 3 26 39 (tz_offset_s 0)) (2094 Apr 8 3 27 52 (tz_offset_s 0))) ((2099 Feb 20 1 34 51 (tz_offset_s 0)) (2099 Feb 20 1 34 58 (tz_offset_s 0))) ((2099 Mar 7 1 15 49 (tz_offset_s 0)) (2099 Mar 7 1 17 23 (tz_offset_s 0)))))
|}
  in
  let timere = Result.get_ok @@ Timere.of_sexp_string s in
  let search_start_dt =
    Result.get_ok
    @@ Timere.Date_time.make ~year:2000 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz_offset_s:0
  in
  let search_start = Timere.Date_time.to_timestamp search_start_dt in
  let search_end_exc_dt =
    Result.get_ok
    @@ Timere.Date_time.make ~year:2001 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz_offset_s:0
  in
  let search_end_exc = Timere.Date_time.to_timestamp search_end_exc_dt in
  let timere =
    Timere.(inter timere (interval_exc search_start search_end_exc))
  in
  ( match Timere.resolve timere with
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
          print_newline () ) );
  let s =
    Timere.Utils.resolve_simple ~search_start ~search_end_exc ~tz_offset_s:0
      timere
  in
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
    print_newline ()

(* let () = debug_branching () *)

(* let () =
 *   debug_parsing () *)

let () = debug_resolver ()
