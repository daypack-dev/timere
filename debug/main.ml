let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {wday:Xxx} {hour:0X}:{min:0X}:{sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {swday:Xxx} {shour:0X}:{smin:0X}:{ssec:0X}, \
   {eyear} {emon:Xxx} {emday:0X} {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X})"

(* let debug_parsing () =
 *   let expr = "jan to feb, apr" in
 *   match Time_parse.timere expr with
 *   | Error msg -> print_endline msg
 *   | Ok timere -> print_endline (Time.to_sexp_string timere) *)

let debug_resolver () =
  let s = {|
(unchunk (chunk_again (chunk_at_year_boundary (chunk_again (chunk_disjoint_intervals (chunk_again (chunk_at_year_boundary (chunk_disjoint_intervals (empty)))))))))
  |} in
  let timere = Result.get_ok @@ Of_sexp.of_sexp_string s in
  (* let timere =
   *   (fun max_height max_branching randomness ->
   *      Builder.build ~min_year:2000 ~max_year_inc:2002 ~max_height ~max_branching
   *        ~randomness)
   *     3 1 [ 497 ]
   * in *)
  (* let timere =
   *   Time.inter
   *     [
   *       Time.empty;
   *       Time.pattern ~strict:true ~months:[ `Mar ] ~month_days:[ 31 ] ();
   *     ]
   * in *)
  (* let timere =
   *   let open Time in
   *   recur
   *     ~year:(every_nth_year 3)
   *     ~month:
   *       (every_nth_month 3)
   *     ~day:(every_nth_day 10)
   *     ( Result.get_ok
   *       @@ Time.Date_time.make ~year:2000 ~month:`Jan ~day:1 ~hour:0 ~minute:0
   *         ~second:0 ~tz_offset_s:0 )
   * in *)
  (* let timere =
   *   let open Time in
   *   (\* hms_interval_exc
   *    *   (make_hms_exn ~hour:23 ~minute:0 ~second:0)
   *    *   (make_hms_exn ~hour:3 ~minute:0 ~second:0) *\)
   *   change_tz (Time.Time_zone.make "Australia/Sydney")
   *     (months [`Jun])
   * in *)
  print_endline (To_sexp.to_sexp_string timere);
  print_endline "=====";
  let search_start_dt =
    Time.Date_time.make ~year:2000 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz:Time_zone.utc
    |> Result.get_ok
  in
  let search_start =
    Time.Date_time.to_timestamp search_start_dt
    |> Time.Date_time.min_of_timestamp_local_result
    |> Option.get
  in
  let search_end_exc_dt =
    Time.Date_time.make ~year:2003 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz:Time_zone.utc
    |> Result.get_ok
  in
  let search_end_exc =
    Time.Date_time.to_timestamp search_end_exc_dt
    |> Time.Date_time.max_of_timestamp_local_result
    |> Option.get
  in
  (match
     Resolver.resolve
       Time.(inter [ timere; interval_exc search_start search_end_exc ])
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
               Printers.sprintf_interval ~display_using_tz:Time_zone.utc
                 default_interval_format_string ts
             with
             | Ok s -> Printf.printf "%s\n" s
             | Error msg -> Printf.printf "Error: %s\n" msg)));
  print_endline "=====";
  let s =
    Simple_resolver.resolve ~search_start ~search_end_exc
      ~search_using_tz:Time_zone.utc timere
  in
  (match s () with
   | Seq.Nil -> print_endline "No matching time slots"
   | Seq.Cons _ ->
     s
     |> OSeq.take 50
     |> OSeq.iter (fun ts ->
         match
           Printers.sprintf_interval ~display_using_tz:Time_zone.utc
             default_interval_format_string ts
         with
         | Ok s -> Printf.printf "%s\n" s
         | Error msg -> Printf.printf "Error: %s\n" msg));
  print_newline ()

let debug_ccsexp_parse_string () = CCSexp.parse_string "\"\\256\"" |> ignore

(* let () = debug_branching () *)

(* let () = debug_parsing () *)

let () = debug_resolver ()

(* let () = debug_ccsexp_parse_string () *)
