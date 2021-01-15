let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {wday:Xxx} {hour:0X}:{min:0X}:{sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {swday:Xxx} {shour:0X}:{smin:0X}:{ssec:0X}, \
   {eyear} {emon:Xxx} {emday:0X} {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X})"

let display_intervals ~display_using_tz s =
  match s () with
  | Seq.Nil -> print_endline "No time intervals"
  | Seq.Cons _ ->
    s
    |> OSeq.take 100
    |> OSeq.iter (fun (x, y) ->
        let s = Printers.sprintf_interval ~display_using_tz (x, y) in
        let size = Duration.of_seconds (Int64.sub y x) in
        let size_str = Printers.sprint_duration size in
        Printf.printf "%s - %s\n" s size_str)

let debug_resolver () =
  (*   let s = {|
   * (unchunk (drop 3 (chunk_at_month_boundary (all))))
   *     |} in
   *   let timere = CCResult.get_exn @@ Of_sexp.of_sexp_string s in *)
  (* let timere =
   *   (fun max_height max_branching randomness ->
   *      Builder.build ~min_year:2000 ~max_year_inc:2002 ~max_height ~max_branching
   *        ~randomness)
   *     2 1 [ 231; 495; 914; 495 ]
   * in *)
  let timere =
    let open Time in
    inter
      [
        shift
          (Duration.make ~days:366 ())
          (pattern ~years:[ 2020 ] ~months:[ `Jan ] ~month_days:[ 1 ] ());
        pattern ~years:[ 2021 ] ~months:[ `Jan ] ~month_days:[ 1 ] ();
      ]
  in
  (* let timere =
   *   let open Time in
   *   recur
   *     ~year:(every_nth_year 3)
   *     ~month:
   *       (every_nth_month 3)
   *     ~day:(every_nth_day 10)
   *     ( Result.get_ok
   *       @@ Time.Date_time'.make ~year:2000 ~month:`Jan ~day:1 ~hour:0 ~minute:0
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
    Time.Date_time'.make ~year:2000 ~month:`Jan ~day:1 ~hour:10 ~minute:0
      ~second:0 ~tz
    |> CCResult.get_exn
  in
  let search_start =
    Time.Date_time'.to_timestamp search_start_dt
    |> Time.Date_time'.min_of_timestamp_local_result
    |> CCOpt.get_exn
  in
  let search_end_exc_dt =
    Time.Date_time'.make ~year:2003 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz
    |> CCResult.get_exn
  in
  let search_end_exc =
    Time.Date_time'.to_timestamp search_end_exc_dt
    |> Time.Date_time'.max_of_timestamp_local_result
    |> CCOpt.get_exn
  in
  let timere' =
    Time.(inter [ timere; of_intervals [ (search_start, search_end_exc) ] ])
  in
  print_endline "^^^^^";
  print_endline (To_sexp.to_sexp_string timere');
  print_endline "=====";
  (match Resolver.resolve timere with
   | Error msg -> print_endline msg
   | Ok s -> display_intervals ~display_using_tz:tz s);
  print_endline "=====";
  (* let s =
   *   Simple_resolver.resolve ~search_start ~search_end_exc
   *     ~search_using_tz:Time_zone.utc timere
   * in
   * display_intervals ~display_using_tz:tz s; *)
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
           years [ 2020; 2021; 2022; 2023; 2025; 2026 ] (* in year 2020 *);
           union
             [
               pattern ~months:[ `Apr ]
                 ~month_day_ranges:[ `Range_inc (3, 6) ]
                 ()
               (* in April 3 to 6 *);
               (* pattern ~months:[`Oct] ~month_day_ranges:[`Range_inc (2, 5)] () (\* or in Oct 2 to 5 *\); *)
             ];
           (* hms_interval_exc (\* 11pm to 3am *\)
            *   (make_hms_exn ~hour:23 ~minute:0 ~second:0)
            *   (make_hms_exn ~hour:3 ~minute:0 ~second:0); *)
         ])
  in
  match Resolver.resolve timere with
  | Error msg -> print_endline msg
  | Ok s -> display_intervals ~display_using_tz:tz s

let debug_fuzz_bounded_intervals () =
  let tz = Time_zone.utc in
  let bound = Int64.of_int 1064 in
  let p1 =
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_points ~rng ~min_year ~max_year_inc ~max_precision:7)
      []
  in
  let p2 =
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_points ~rng ~min_year ~max_year_inc
         ~max_precision:(Points.precision p1))
      []
  in
  let s1 = Resolver.aux_points tz Resolver.default_search_space p1 in
  let s2 = Resolver.aux_points tz Resolver.default_search_space p2 in
  let s =
    Resolver.(
      aux_bounded_intervals tz Resolver.default_search_space `Whole bound p1 p2)
  in
  let s' =
    Resolver.(
      aux_bounded_intervals tz Resolver.default_search_space `Snd bound p1 p2)
  in
  print_endline "====="

let debug_fuzz_union () =
  let tz = Time_zone.utc in
  let t1 =
    (fun max_height max_branching randomness ->
       let max_height = 1 + max_height in
       let max_branching = 1 + max_branching in
       Builder.build ~enable_extra_restrictions:false ~min_year:2000
         ~max_year_inc:2002 ~max_height ~max_branching ~randomness)
      0 3 [ 761; 143 ]
  in
  let t2 =
    (fun max_height max_branching randomness ->
       let max_height = 1 + max_height in
       let max_branching = 1 + max_branching in
       Builder.build ~enable_extra_restrictions:false ~min_year:2000
         ~max_year_inc:2002 ~max_height ~max_branching ~randomness)
      1 0 [ 113 ]
  in
  let t1' = Resolver.t_of_ast t1 in
  let t2' = Resolver.t_of_ast t2 in
  let s1 = Resolver.aux tz t1' |> Resolver.normalize in
  let s2 = Resolver.aux tz t2' in
  let l = [ t1' ] in
  let s' =
    l
    |> List.map (Resolver.aux tz)
    |> CCList.to_seq
    |> Time.Intervals.Union.union_multi_seq
    |> Time.Intervals.Slice.slice ~start:Time.min_timestamp
      ~end_exc:Time.max_timestamp
  in
  let s = Resolver.aux_union tz (CCList.to_seq l) |> Resolver.normalize in
  print_endline "=====";
  display_intervals ~display_using_tz:tz s1;
  print_endline (To_sexp.to_sexp_string t1);
  (* print_endline "=====";
   * display_intervals ~display_using_tz:tz s2;
   * print_endline (To_sexp.to_sexp_string t2); *)
  print_endline "=====";
  display_intervals ~display_using_tz:tz s';
  print_endline "=====";
  display_intervals ~display_using_tz:tz s;
  print_endline "=====";
  Printf.printf "%b\n" (OSeq.equal ~eq:( = ) s s')

(* let () = debug_branching () *)

(* let () = debug_parsing () *)

let () = debug_fuzz_bounded_intervals ()

(* let () = debug_resolver () *)

(* let () = debug_ccsexp_parse_string () *)

(* let () = debug_example () *)

(* let () = debug_fuzz_after () *)

(* let () = debug_fuzz_between_exc () *)

(* let () = debug_fuzz_union () *)
