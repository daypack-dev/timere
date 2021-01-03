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

let display_intervals ~display_using_tz s =
  match s () with
  | Seq.Nil -> print_endline "No time intervals"
  | Seq.Cons _ ->
    s
    |> OSeq.take 60
    |> OSeq.iter (fun (x, y) ->
        let s = Printers.sprintf_interval ~display_using_tz (x, y) in
        let size = Duration.of_seconds (Int64.sub y x) in
        let size_str = Printers.sprint_duration size in
        Printf.printf "%s - %s\n" s size_str)

let debug_resolver () =
  let s =
    {|
(take_n_points 4 (between_exc (duration 1 0 0 0) (pattern (hours 23) (minutes 2) (seconds 59)) (pattern (hours 2) (minutes 59) (seconds 2))))
    |}
  in
  let timere = Result.get_ok @@ Of_sexp.of_sexp_string s in
  (* let timere =
   *   (fun max_height max_branching randomness ->
   *      Builder.build ~min_year:2000 ~max_year_inc:2002 ~max_height ~max_branching
   *        ~randomness)
   *     3 2 [ 119; 962 ]
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
  (* let tz = Time_zone.make_exn "Australia/Sydney" in *)
  let tz = Time_zone.make_exn "UTC" in
  (* let timere =
   *   let open Time in
   *   with_tz tz
   *     (inter
   *        [
   *          years [ 2020 ];
   *          (\* between_exc (month_days [ -1 ]) (month_days [ 1 ]); *\)
   *          (\* always; *\)
   *          hms_interval_exc
   *            (make_hms_exn ~hour:1 ~minute:15 ~second:0)
   *            (make_hms_exn ~hour:2 ~minute:30 ~second:0);
   *          months [ `Oct ];
   *        ])
   *     (\* (pattern ~months:[`Mar] ~hours:[23] ~minutes:[0] ~seconds:[0]()) *\)
   *     (\* (pattern ~months:[`Mar] ~hours:[4] ~minutes:[30] ~seconds:[0]()) *\)
   * in *)
  print_endline (To_sexp.to_sexp_string timere);
  let search_start_dt =
    Time.Date_time.make ~year:0 ~month:`Jan ~day:1 ~hour:10 ~minute:0
      ~second:0 ~tz
    |> Result.get_ok
  in
  let search_start =
    Time.Date_time.to_timestamp search_start_dt
    |> Time.Date_time.min_of_timestamp_local_result
    |> Option.get
  in
  let search_end_exc_dt =
    Time.Date_time.make ~year:3 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz
    |> Result.get_ok
  in
  let search_end_exc =
    Time.Date_time.to_timestamp search_end_exc_dt
    |> Time.Date_time.max_of_timestamp_local_result
    |> Option.get
  in
  let timere' =
    Time.(inter [ timere; interval_exc search_start search_end_exc ])
  in
  print_endline "^^^^^";
  print_endline (To_sexp.to_sexp_string timere');
  print_endline "=====";
  (match
     Resolver.resolve
       timere'
   with
   | Error msg -> print_endline msg
   | Ok s -> display_intervals ~display_using_tz:tz s);
  print_endline "=====";
  let s =
    Simple_resolver.resolve ~search_start ~search_end_exc
      ~search_using_tz:Time_zone.utc timere
  in
  display_intervals ~display_using_tz:tz s;
  print_newline ()

let debug_ccsexp_parse_string () = CCSexp.parse_string "\"\\256\"" |> ignore

let debug_example () =
  let display_intervals ~display_using_tz s =
    match s () with
    | Seq.Nil -> print_endline "No time intervals"
    | Seq.Cons _ ->
      s
      |> OSeq.take 60
      |> OSeq.iter (fun (x, y) ->
          let s = Printers.sprintf_interval ~display_using_tz (x, y) in
          let size = Duration.of_seconds (Int64.sub y x) in
          let size_str = Printers.sprint_duration size in
          Printf.printf "%s - %s\n" s size_str)
  in
  let tz = Time_zone.make_exn "Australia/Sydney" in
  let timere =
    let open Time in
    with_tz tz
      (inter
         [
           years [ 2020 ] (* in year 2020 *);
           (* union [
            *   pattern ~months:[`Apr] ~month_day_ranges:[`Range_inc (3, 6)] () (\* in April 3 to 6 *\);
            *   (\* pattern ~months:[`Oct] ~month_day_ranges:[`Range_inc (2, 5)] () (\\* or in Oct 2 to 5 *\\); *\)
            * ];
            * hms_interval_exc (\* 11pm to 3am *\)
            *   (make_hms_exn ~hour:23 ~minute:0 ~second:0)
            *   (make_hms_exn ~hour:3 ~minute:0 ~second:0); *)
         ])
  in
  match Resolver.resolve timere with
  | Error msg -> print_endline msg
  | Ok s -> display_intervals ~display_using_tz:tz s

(* let () = debug_branching () *)

(* let () = debug_parsing () *)

let () = debug_resolver ()

(* let () = debug_ccsexp_parse_string () *)

(* let () = debug_example () *)
