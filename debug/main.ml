let default_date_time_format_string =
  "{year} {mon:Xxx} {day:0X} {wday:Xxx} \
   {hour:0X}:{min:0X}:{sec:0X}.{sec-frac:9}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {sday:0X} {swday:Xxx} \
   {shour:0X}:{smin:0X}:{ssec:0X}.{ssec-frac:9}, {eyear} {emon:Xxx} {eday:0X} \
   {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X}.{esec-frac:9})"

let display_timestamps ~display_using_tz s =
  match s () with
  | Seq.Nil -> print_endline "No timestamps"
  | Seq.Cons _ ->
    s
    |> OSeq.take 20
    |> OSeq.iter (fun x ->
        let s =
          Timedesc.Timestamp.to_string ~display_using_tz
            ~format:default_date_time_format_string x
        in
        Printf.printf "%s\n" s;
        flush stdout)

let display_intervals ~display_using_tz s =
  match s () with
  | Seq.Nil -> print_endline "No time intervals"
  | Seq.Cons _ ->
    s
    |> OSeq.take 20
    |> OSeq.iter (fun (x, y) ->
        let s =
          Timedesc.Interval.to_string ~display_using_tz
            ~format:default_interval_format_string (x, y)
        in
        let size = Timedesc.Span.sub y x in
        let size_str = Timedesc.Span.For_human.to_string size in
        Printf.printf "%s - %s\n" s size_str;
        flush stdout)

let debug_resolver () =
  let s =
    {|
(with_tz UTC+1:00
 (bounded_intervals whole (31622400 0)
  (points (pick ymdhms 2001 May 10 10 0 0)) (points (pick ymdhms 2001 May 10 14 0 0)))
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
  (* let tz = Time_zone.make_exn "Europe/Paris" in *)
  let tz = Timedesc.Time_zone.make_exn "UTC" in
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
    Timedesc.make_exn ~year:2000 ~month:1 ~day:1 ~hour:10 ~minute:0 ~second:0
      ~tz ()
  in
  let search_start =
    Timedesc.to_timestamp search_start_dt |> Timedesc.min_of_local_result
  in
  let search_end_exc_dt =
    Timedesc.make_exn ~year:2003 ~month:1 ~day:1 ~hour:0 ~minute:0 ~second:0 ~tz
      ()
  in
  let search_end_exc =
    Timedesc.to_timestamp search_end_exc_dt |> Timedesc.max_of_local_result
  in
  let timere' =
    Time.(
      inter
        [
          timere;
          (* after
           *   (Date_time'.make_exn ~tz ~year:1999 ~month:`Jan ~day:1 ~hour:0
           *      ~minute:0 ~second:0 ());
           * before
           *   (Date_time'.make_exn ~tz ~year:2010 ~month:`Jan ~day:1 ~hour:0
           *      ~minute:0 ~second:0 ()); *)
          intervals [ (search_start, search_end_exc) ];
        ])
  in
  print_endline "^^^^^";
  print_endline (To_sexp.to_sexp_string timere');
  print_endline "=====";
  (match Resolver.resolve ~search_using_tz:tz timere' with
   | Error msg -> print_endline msg
   | Ok s -> display_intervals ~display_using_tz:Timedesc.Time_zone.utc s);
  print_endline "=====";
  let s =
    Simple_resolver.resolve ~search_start ~search_end_exc ~search_using_tz:tz
      timere
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
          let s = Timedesc.Interval.to_string ~display_using_tz (x, y) in
          let size = Timedesc.Span.sub y x in
          let size_str = Timedesc.Span.For_human.to_string size in
          Printf.printf "%s - %s\n" s size_str)
  in
  (* let tz = Time_zone.make_exn "Australia/Sydney" in *)
  let tz = Timedesc.Time_zone.utc in
  let timere =
    let open Time in
    with_tz tz
      (inter
         [
           CCResult.get_exn
           @@ Of_sexp.of_sexp_string
             "(bounded_intervals whole (duration 366 0 0 0) (points (pick \
              ymdhms 2020 Jun 16 10 0 0)) (points (pick dhms 17 12 0 0)))";
           after
             (Timedesc.make_exn ~tz ~year:2000 ~month:1 ~day:1 ~hour:0 ~minute:0
                ~second:0 ());
           before
             (Timedesc.make_exn ~tz ~year:2050 ~month:1 ~day:1 ~hour:0 ~minute:0
                ~second:0 ());
         ])
  in
  match Resolver.resolve timere with
  | Error msg -> print_endline msg
  | Ok s -> display_intervals ~display_using_tz:tz s

let debug_fuzz_bounded_intervals () =
  let tz_count = List.length Timedesc.Time_zone.available_time_zones in
  let tz =
    (fun n ->
       let n = max 0 n mod tz_count in
       Timedesc.Time_zone.make_exn
         (List.nth Timedesc.Time_zone.available_time_zones n))
      (-578721249560635033)
  in
  let bound = Timedesc.Span.make ~s:51753L () in
  let p1 =
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_points ~rng ~min_year ~max_year_inc ~max_precision:7)
      [ 4073; 0 ]
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
  Printf.printf "p1: %s\n" (To_sexp.sexp_of_points p1 |> CCSexp.to_string);
  Printf.printf "p2: %s\n" (To_sexp.sexp_of_points p2 |> CCSexp.to_string);
  print_endline "=====";
  display_timestamps ~display_using_tz:tz s1;
  print_endline "=====";
  display_timestamps ~display_using_tz:tz s2;
  print_endline "=====";
  display_intervals ~display_using_tz:tz s;
  print_endline "=====";
  display_intervals ~display_using_tz:tz s';
  print_endline "=====";
  Printf.printf "%b\n"
    (OSeq.for_all
       (fun x1 ->
          match
            Seq.filter
              Timedesc.Span.(fun x2 -> x1 < x2 && x2 - x1 <= bound)
              s2 ()
          with
          | Seq.Nil -> true
          | Seq.Cons (xr2, _) ->
            if
              OSeq.mem ~eq:( = ) (x1, xr2) s
              && OSeq.mem ~eq:( = ) (xr2, Timedesc.Span.succ xr2) s'
            then true
            else (
              print_endline
                (Timedesc.Timestamp.to_string ~display_using_tz:tz xr2);
              false))
       s1)

let debug_fuzz_union () =
  let tz = Timedesc.Time_zone.utc in
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
    |> Time.Intervals.Slice.slice ~start:Timedesc.Timestamp.min_val
      ~end_exc:Timedesc.Timestamp.max_val
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
  let tz_count = List.length Timedesc.Time_zone.available_time_zones in
  let tz =
    (fun n ->
       let n = max 0 n mod tz_count in
       Timedesc.Time_zone.make_exn
         (List.nth Timedesc.Time_zone.available_time_zones n))
      4014879592515549111
  in
  print_endline (Timedesc.Time_zone.name tz);
  let search_space =
    List.map
      (fun (search_start, search_size) ->
         let search_start =
           min
             (max Timedesc.Timestamp.min_val search_start)
             Timedesc.Timestamp.max_val
         in
         let search_size = Timedesc.Span.make ~s:(Int64.abs search_size) () in
         let search_end_exc =
           min Timedesc.Timestamp.max_val
             (Timedesc.Span.add search_start search_size)
         in
         (search_start, search_end_exc))
      [ (Timedesc.Span.make ~s:(-5208492133891178625L) (), 201999689168823L) ]
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
          ~a:Timedesc.Span.((fst (List.hd search_space)).s)
          ~b:
            (snd
               (CCOpt.get_exn_or
                  "Expected successful retrieval of last element of list"
                @@ Misc_utils.last_element_of_list search_space))
            .s
        |> OSeq.filter (fun timestamp ->
            List.exists
              (fun (x, y) ->
                 Timedesc.Span.(x.s) <= timestamp
                 && timestamp < Timedesc.Span.(y.s))
              search_space)
        |> OSeq.filter (fun timestamp ->
            let dt =
              CCOpt.get_exn_or
                "Expected successful construction of date time"
              @@ Timedesc.of_timestamp ~tz_of_date_time:tz
                (Timedesc.Span.make ~s:timestamp ())
            in
            let weekday = Timedesc.weekday dt in
            let year_is_fine =
              Int_set.is_empty pattern.years
              || Int_set.mem (Timedesc.year dt) pattern.years
            in
            let month_is_fine =
              Int_set.is_empty pattern.months
              || Int_set.mem (Timedesc.month dt) pattern.months
            in
            let mday_is_fine =
              Int_set.is_empty pattern.month_days
              ||
              let day_count =
                Timedesc.Utils.day_count_of_month ~year:(Timedesc.year dt)
                  ~month:(Timedesc.month dt)
              in
              pattern.month_days
              |> Int_set.to_seq
              |> Seq.map (fun mday ->
                  if mday < 0 then day_count + mday + 1 else mday)
              |> OSeq.mem ~eq:( = ) (Timedesc.day dt)
            in
            let wday_is_fine =
              Weekday_set.is_empty pattern.weekdays
              || Weekday_set.mem weekday pattern.weekdays
            in
            let hour_is_fine =
              Int_set.is_empty pattern.hours
              || Int_set.mem (Timedesc.hour dt) pattern.hours
            in
            let minute_is_fine =
              Int_set.is_empty pattern.minutes
              || Int_set.mem (Timedesc.minute dt) pattern.minutes
            in
            let second_is_fine =
              Int_set.is_empty pattern.seconds
              || Int_set.mem (Timedesc.second dt) pattern.seconds
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
           if
             OSeq.exists
               (fun (x, y) ->
                  Timedesc.Span.(x.s) <= x' && x' < Timedesc.Span.(y.s))
               s
           then true
           else (
             Printf.printf "x': %Ld\n" x';
             false))
        s'
  in
  Printf.printf "%b\n" r

(* let () = debug_branching () *)

(* let () = debug_parsing () *)

(* let () = debug_fuzz_bounded_intervals () *)

let () = debug_resolver ()

(* let () = debug_fuzz_pattern () *)

(* let () = debug_ccsexp_parse_string () *)

(* let () = debug_example () *)

(* let () = debug_fuzz_after () *)

(* let () = debug_fuzz_between_exc () *)

(* let () = debug_fuzz_union () *)
