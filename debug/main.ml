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
    |> OSeq.take 50
    |> OSeq.iter (fun (x, y) ->
        let s = Printers.string_of_interval ~display_using_tz (x, y) in
        let size = Duration.of_seconds (Int64.sub y x) in
        let size_str = Printers.string_of_duration size in
        Printf.printf "%s - %s\n" s size_str)

let debug_resolver () =
  let s =
    {|
(shift 1
  (intervals
    ((2002 Jun 9 20 15 29 (tz_and_tz_offset_s UTC 0))
     (2002 Jun 9 20 18 34 (tz_and_tz_offset_s UTC 0))
    ))
)
      |}
  in
  let timere = CCResult.get_exn @@ Of_sexp.of_sexp_string s in
  (* let timere =
   *   (fun max_height max_branching randomness ->
   *      let max_height = 1 + max_height in
   *      let max_branching = 1 + max_branching in
   *      Builder.build ~min_year:2000 ~max_year_inc:2002 ~max_height ~max_branching
   *        ~randomness ~enable_extra_restrictions:false)
   *     1 1 [ 15; 449; 968; 185 ]
   * in *)
  (* let timere =
   *   let open Time in
   *   inter
   *     [
   *       shift
   *         (Duration.make ~days:366 ())
   *         (pattern ~years:[ 2020 ] ~months:[ `Jan ] ~month_days:[ 1 ] ());
   *       pattern ~years:[ 2021 ] ~months:[ `Jan ] ~month_days:[ 1 ] ();
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
    |> CCOpt.get_exn
  in
  let search_start =
    Time.Date_time'.to_timestamp search_start_dt
    |> Time.Date_time'.min_of_timestamp_local_result
  in
  let search_end_exc_dt =
    Time.Date_time'.make ~year:2003 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz
    |> CCOpt.get_exn
  in
  let search_end_exc =
    Time.Date_time'.to_timestamp search_end_exc_dt
    |> Time.Date_time'.max_of_timestamp_local_result
  in
  let timere' =
    Time.(inter [ timere; intervals [ (search_start, search_end_exc) ] ])
  in
  print_endline "^^^^^";
  print_endline (To_sexp.to_sexp_string timere');
  print_endline "=====";
  (match Resolver.resolve timere' with
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
          let s = Printers.string_of_interval ~display_using_tz (x, y) in
          let size = Duration.of_seconds (Int64.sub y x) in
          let size_str = Printers.string_of_duration size in
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
               pattern ~months:[ `Apr ] ~day_ranges:[ `Range_inc (3, 6) ] ()
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
  let tz_count = List.length Time_zone.available_time_zones in
  let tz =
    (fun n ->
       let n = max 0 n mod tz_count in
       Time_zone.make_exn (List.nth Time_zone.available_time_zones n))
      140733971657571
  in
  let bound = Int64.of_int 82400 in
  let p1 =
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_points ~rng ~min_year ~max_year_inc ~max_precision:7)
      [ 3779; 0 ]
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
  display_intervals ~display_using_tz:tz s;
  print_endline "=====";
  display_intervals ~display_using_tz:tz s';
  print_endline "=====";
  Printf.printf "%b\n"
    (OSeq.for_all
       (fun x1 ->
          match
            Seq.filter (fun x2 -> x1 < x2 && Int64.sub x2 x1 <= bound) s2 ()
          with
          | Seq.Nil -> true
          | Seq.Cons (xr2, _) ->
            if
              OSeq.mem ~eq:( = ) (x1, xr2) s
              && OSeq.mem ~eq:( = ) (xr2, Int64.succ xr2) s'
            then true
            else (
              print_endline
                (Printers.string_of_timestamp ~display_using_tz:tz xr2);
              false))
       s1)

let debug_fuzz_union () =
  let tz = Time_zone.utc in
  (* let t1 =
   *   (fun max_height max_branching randomness ->
   *      let max_height = 1 + max_height in
   *      let max_branching = 1 + max_branching in
   *      Builder.build ~enable_extra_restrictions:false ~min_year:2000
   *        ~max_year_inc:2002 ~max_height ~max_branching ~randomness)
   *     1 1 [265; 47; 268; 6]
   * in *)
  let t1 =
    let s =
      {|
(with_tz UTC (bounded_intervals whole (duration 1 0 0 0) (points (pick hms 1 6 28)) (points (pick hms 23 25 7))))
      |}
    in
    CCResult.get_exn @@ Of_sexp.of_sexp_string s
  in
  let t2 =
    (fun max_height max_branching randomness ->
       let max_height = 1 + max_height in
       let max_branching = 1 + max_branching in
       Builder.build ~enable_extra_restrictions:false ~min_year:2000
         ~max_year_inc:2002 ~max_height ~max_branching ~randomness)
      1 3 [ 613; 937; 937 ]
  in
  let t1' = Resolver.t_of_ast t1 in
  let t2' = Resolver.t_of_ast t2 in
  print_endline (To_sexp.to_sexp_string t1);
  let s1 = Resolver.aux tz t1' |> Resolver.normalize in
  let s2 = Resolver.aux tz t2' in
  (match Resolver.resolve t1 with
   | Error msg ->
     print_endline msg;
     flush stdout
   | _ -> ());
  let s1 = CCResult.get_exn @@ Resolver.resolve t1 in
  let s2 = CCResult.get_exn @@ Resolver.resolve t2 in
  let l = [ t1' ] in
  let s' =
    l
    |> List.map (Resolver.aux tz)
    |> CCList.to_seq
    |> Time.Intervals.Union.union_multi_seq
    |> Time.Intervals.Slice.slice ~start:Time.timestamp_min
      ~end_exc:Time.timestamp_max
  in
  let s = Resolver.aux_union tz (CCList.to_seq l) |> Resolver.normalize in
  print_endline "=====";
  display_intervals ~display_using_tz:tz s1;
  (* print_endline "=====";
   * display_intervals ~display_using_tz:tz s2;
   * print_endline (To_sexp.to_sexp_string t2); *)
  print_endline "=====";
  display_intervals ~display_using_tz:tz s';
  print_endline "=====";
  display_intervals ~display_using_tz:tz s;
  print_endline "=====";
  Printf.printf "%b\n" (OSeq.equal ~eq:( = ) s s')

let debug_fuzz_pattern () =
  let open Date_time_components in
  let tz_count = List.length Time_zone.available_time_zones in
  let tz =
    (fun n ->
       let n = max 0 n mod tz_count in
       Time_zone.make_exn (List.nth Time_zone.available_time_zones n))
      4014879592515549111
  in
  print_endline (Time_zone.name tz);
  let search_space =
    List.map
      (fun (search_start, search_size) ->
         let search_start =
           min (max Time.timestamp_min search_start) Time.timestamp_max
         in
         let search_size = Int64.abs search_size in
         let search_end_exc =
           min Time.timestamp_max (Int64.add search_start search_size)
         in
         (search_start, search_end_exc))
      [ (-5208492133891178625L, 201999689168823L) ]
  in
  let pattern =
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_pattern ~rng ~min_year ~max_year_inc)
      []
  in
  print_endline (CCSexp.to_string (To_sexp.sexp_of_pattern pattern));
  let s = Resolver.aux_pattern tz search_space pattern |> Resolver.normalize in
  let r =
    match search_space with
    | [] -> OSeq.is_empty s
    | _ ->
      let s' =
        Seq_utils.a_to_b_exc_int64
          ~a:(fst (List.hd search_space))
          ~b:
            (snd
               (CCOpt.get_exn @@ Misc_utils.last_element_of_list search_space))
        |> OSeq.filter (fun timestamp ->
            List.exists
              (fun (x, y) -> x <= timestamp && timestamp < y)
              search_space)
        |> OSeq.filter (fun timestamp ->
            let dt =
              CCOpt.get_exn
              @@ Time.Date_time'.of_timestamp ~tz_of_date_time:tz timestamp
            in
            let weekday =
              CCOpt.get_exn
              @@ weekday_of_month_day ~year:dt.year ~month:dt.month
                ~mday:dt.day
            in
            let year_is_fine =
              Int_set.is_empty pattern.years
              || Int_set.mem dt.year pattern.years
            in
            let month_is_fine =
              Month_set.is_empty pattern.months
              || Month_set.mem dt.month pattern.months
            in
            let mday_is_fine =
              Int_set.is_empty pattern.month_days
              ||
              let day_count =
                day_count_of_month ~year:dt.year ~month:dt.month
              in
              pattern.month_days
              |> Int_set.to_seq
              |> Seq.map (fun mday ->
                  if mday < 0 then day_count + mday + 1 else mday)
              |> OSeq.mem ~eq:( = ) dt.day
            in
            let wday_is_fine =
              Weekday_set.is_empty pattern.weekdays
              || Weekday_set.mem weekday pattern.weekdays
            in
            let hour_is_fine =
              Int_set.is_empty pattern.hours
              || Int_set.mem dt.hour pattern.hours
            in
            let minute_is_fine =
              Int_set.is_empty pattern.minutes
              || Int_set.mem dt.minute pattern.minutes
            in
            let second_is_fine =
              Int_set.is_empty pattern.seconds
              || Int_set.mem dt.second pattern.seconds
            in
            year_is_fine
            && month_is_fine
            && mday_is_fine
            && wday_is_fine
            && hour_is_fine
            && minute_is_fine
            && second_is_fine)
      in
      OSeq.for_all
        (fun x' ->
           if OSeq.exists (fun (x, y) -> x <= x' && x' < y) s then true
           else (
             Printf.printf "x': %Ld\n" x';
             false))
        s'
  in
  Printf.printf "%b\n" r

(* let () = debug_branching () *)

(* let () = debug_parsing () *)

let () = debug_fuzz_bounded_intervals ()

(* let () = debug_resolver () *)

(* let () = debug_fuzz_pattern () *)

(* let () = debug_ccsexp_parse_string () *)

(* let () = debug_example () *)

(* let () = debug_fuzz_after () *)

(* let () = debug_fuzz_between_exc () *)

(* let () = debug_fuzz_union () *)
