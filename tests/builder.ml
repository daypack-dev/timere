let make_rng ~randomness : unit -> int =
  let randomness = match randomness with [] -> [ 0 ] | _ -> randomness in
  let arr = Array.of_list randomness in
  let len = Array.length arr in
  let cur = ref 0 in
  fun () ->
    let ret = arr.(!cur) in
    cur := (!cur + 1) mod len;
    ret

let make_date_time ~rng ~min_year ~max_year_inc =
  let year = min max_year_inc (min_year + rng ()) in
  let month = succ (rng () mod 12) in
  let day = 1 + (rng () mod Timedesc.Utils.day_count_of_month ~year ~month) in
  let hour = rng () mod 24 in
  let minute = rng () mod 60 in
  let second = rng () mod 60 in
  let available_time_zone_count =
    List.length Timedesc.Time_zone.available_time_zones
  in
  let tz =
    List.nth Timedesc.Time_zone.available_time_zones
      (rng () mod available_time_zone_count)
    |> Timedesc.Time_zone.make_exn
  in
  match Timedesc.make ~year ~month ~day ~hour ~minute ~second ~tz () with
  | Error _ ->
    Timedesc.make_exn ~year ~month ~day ~hour ~minute ~second
      ~tz:Timedesc.Time_zone.utc ()
  | Ok x -> x

let make_timestamp_intervals ~rng ~min_year ~max_year_inc =
  let len = min 5 (rng ()) in
  OSeq.(0 -- len)
  |> Seq.map (fun _ ->
      let start =
        make_date_time ~rng ~min_year ~max_year_inc
        |> Timedesc.to_timestamp
        |> Timedesc.min_of_local_result
      in
      let end_exc =
        Timedesc.Span.add start (Timedesc.Span.make ~ns:(rng ()) ())
      in
      (start, end_exc))
  |> CCList.of_seq
  |> List.sort_uniq Time.Interval'.compare
  |> CCList.to_seq
  |> Time.interval_seq

let make_pattern ~rng ~min_year ~max_year_inc : Pattern.t =
  let years =
    if rng () mod 2 = 0 then []
    else
      let end_inc = min 5 (rng ()) in
      OSeq.(0 -- end_inc)
      |> Seq.map (fun _ -> min max_year_inc (min_year + rng ()))
      |> CCList.of_seq
  in
  let months =
    if rng () mod 2 = 0 then []
    else
      let end_inc = min 5 (rng ()) in
      OSeq.(0 -- end_inc)
      |> Seq.map (fun _ -> succ (rng () mod 12))
      |> CCList.of_seq
  in
  let month_days =
    if rng () mod 2 = 0 then []
    else
      let end_inc = min 5 (rng ()) in
      OSeq.(0 -- end_inc)
      |> Seq.map (fun _ ->
          if rng () mod 2 = 0 then 1 + (rng () mod 31)
          else -(1 + (rng () mod 31)))
      |> CCList.of_seq
  in
  let weekdays =
    if rng () mod 2 = 0 then []
    else
      let end_inc = min 5 (rng ()) in
      OSeq.(0 -- end_inc)
      |> Seq.map (fun _ ->
          CCOpt.get_exn_or
            "Expected successful weekday construction from tm_int"
          @@ Timedesc.Utils.weekday_of_tm_int (rng () mod 7))
      |> CCList.of_seq
  in
  let hours =
    if rng () mod 2 = 0 then []
    else
      let end_inc = min 5 (rng ()) in
      OSeq.(0 -- end_inc) |> Seq.map (fun _ -> rng () mod 24) |> CCList.of_seq
  in
  let minutes =
    if rng () mod 2 = 0 then []
    else
      let end_inc = min 5 (rng ()) in
      OSeq.(0 -- end_inc) |> Seq.map (fun _ -> rng () mod 60) |> CCList.of_seq
  in
  let seconds =
    if rng () mod 2 = 0 then []
    else
      let end_inc = min 5 (rng ()) in
      OSeq.(0 -- end_inc) |> Seq.map (fun _ -> rng () mod 60) |> CCList.of_seq
  in
  Pattern.
    {
      years = Int_set.of_list years;
      months = Int_set.of_list months;
      month_days = Int_set.of_list month_days;
      weekdays = Weekday_set.of_list weekdays;
      hours = Int_set.of_list hours;
      minutes = Int_set.of_list minutes;
      seconds = Int_set.of_list seconds;
    }

let make_points ~rng ~min_year ~max_year_inc ~max_precision =
  let day =
    if rng () mod 2 = 0 then 1 + (rng () mod 31) else -(1 + (rng () mod 31))
  in
  let precision = min max_precision (rng () mod 7) in
  match precision with
  | 0 -> Points.make_exn ~second:(rng () mod 60) ()
  | 1 -> Points.make_exn ~minute:(rng () mod 60) ~second:(rng () mod 60) ()
  | 2 ->
    Points.make_exn
      ~hour:(rng () mod 24)
      ~minute:(rng () mod 60)
      ~second:(rng () mod 60)
      ()
  | 3 ->
    Points.make_exn
      ~weekday:
        (rng () mod 7
         |> Timedesc.Utils.weekday_of_tm_int
         |> CCOpt.get_exn_or
           "Expected successful weekday construction from tm_int")
      ~hour:(rng () mod 24)
      ~minute:(rng () mod 60)
      ~second:(rng () mod 60)
      ()
  | 4 ->
    Points.make_exn ~day
      ~hour:(rng () mod 24)
      ~minute:(rng () mod 60)
      ~second:(rng () mod 60)
      ()
  | 5 ->
    Points.make_exn
      ~month:(succ (rng () mod 12))
      ~day
      ~hour:(rng () mod 24)
      ~minute:(rng () mod 60)
      ~second:(rng () mod 60)
      ()
  | 6 ->
    Points.make_exn
      ~year:(min max_year_inc (min_year + rng ()))
      ~month:(succ (rng () mod 12))
      ~day
      ~hour:(rng () mod 24)
      ~minute:(rng () mod 60)
      ~second:(rng () mod 60)
      ()
  | _ -> failwith "Unexpected case"

let make_hms ~rng =
  Time.Hms'.make_exn
    ~hour:(rng () mod 24)
    ~minute:(rng () mod 60)
    ~second:(rng () mod 60)

let make_hms_intervals_inc ~rng =
  Time.hms_intervals_inc (make_hms ~rng) (make_hms ~rng)

let make_hms_intervals_exc ~rng =
  Time.hms_intervals_exc (make_hms ~rng) (make_hms ~rng)

let new_height ~rng height =
  assert (height > 1);
  let reduc = 1 + (rng () mod (height - 1)) in
  assert (reduc >= 1);
  height - reduc

let make_duration ~rng =
  let sign = if rng () mod 2 = 0 then `Pos else `Neg in
  let seconds = rng () in
  let ns = 1 + rng () in
  Timedesc.Span.For_human.make_exn ~sign ~seconds ~ns ()

let make_pos_duration ~rng =
  let seconds = rng () in
  let ns = 1 + rng () in
  Timedesc.Span.For_human.make_exn ~seconds ~ns ()

let make_chunking ~rng : Time_ast.chunking =
  match rng () mod 5 with
  | 0 -> `Disjoint_intervals
  | 1 -> `By_duration (make_pos_duration ~rng)
  | 2 -> `By_duration_drop_partial (make_pos_duration ~rng)
  | 3 -> `At_year_boundary
  | 4 -> `At_month_boundary
  | _ -> failwith "Unexpected case"

let make_chunk_selector ~rng : Time_ast.chunked -> Time_ast.chunked =
  let open Infix in
  let rec aux f height =
    if height <= 1 then f
    else
      let f =
        match rng () mod 6 with
        | 0 -> f %> Time.chunk_again (make_chunking ~rng)
        | 1 -> f %> Time.first
        | 2 -> f %> Time.take (rng () mod 5)
        | 3 -> f %> Time.take_nth (1 + (rng () mod 5))
        | 4 -> f %> Time.nth (rng () mod 5)
        | 5 -> f %> Time.drop (rng () mod 5)
        | _ -> failwith "Unexpected case"
      in
      aux f (new_height ~rng height)
  in
  aux CCFun.id 5

let make_unary_op ~min_year:_ ~max_year_inc:_ ~rng t =
  match rng () mod 4 with
  | 0 -> Time.not t
  (* | 1 ->
   *   Time.drop_points
   *     (rng () mod 5)
   *     Time.(
   *       inter
   *         [
   *           pattern ~year_ranges:[ `Range_inc (min_year, max_year_inc) ] (); t;
   *         ])
   * | 2 -> Time.take_points (rng () mod 5) t *)
  | 1 -> Time.shift (make_duration ~rng) t
  | 2 -> Time.lengthen (make_pos_duration ~rng) t
  | 3 ->
    let available_time_zone_count =
      List.length Timedesc.Time_zone.available_time_zones
    in
    let tz =
      List.nth Timedesc.Time_zone.available_time_zones
        (rng () mod available_time_zone_count)
      |> Timedesc.Time_zone.make_exn
    in
    Time.with_tz tz t
  | _ -> failwith "Unexpected case"

let build ~enable_extra_restrictions:_ ~min_year ~max_year_inc ~max_height
    ~max_branching ~(randomness : int list) : Time_ast.t =
  let rng = make_rng ~randomness in
  let rec aux height =
    if height <= 1 then
      match rng () mod 6 with
      | 0 -> Time.empty
      | 1 -> Time.always
      | 2 -> make_timestamp_intervals ~rng ~min_year ~max_year_inc
      | 3 ->
        let pat = make_pattern ~rng ~min_year ~max_year_inc in
        Time.pattern
          ~years:(Int_set.to_list pat.years)
          ~months:(Int_set.to_list pat.months)
          ~days:(Int_set.to_list pat.month_days)
          ~weekdays:(Weekday_set.to_list pat.weekdays)
          ~hours:(Int_set.to_list pat.hours)
          ~minutes:(Int_set.to_list pat.minutes)
          ~seconds:(Int_set.to_list pat.seconds)
          ()
      | 4 -> make_hms_intervals_inc ~rng
      | 5 -> make_hms_intervals_exc ~rng
      | _ -> failwith "Unexpected case"
    else
      match rng () mod 5 with
      | 0 ->
        make_unary_op ~min_year ~max_year_inc ~rng
          (aux (new_height ~rng height))
      | 1 ->
        let end_inc = min max_branching (rng ()) in
        OSeq.(0 -- end_inc)
        |> Seq.map (fun _ -> aux (new_height ~rng height))
        |> CCList.of_seq
        |> Time.inter
      | 2 ->
        let end_inc = min max_branching (rng ()) in
        OSeq.(0 -- end_inc)
        |> Seq.map (fun _ -> aux (new_height ~rng height))
        |> CCList.of_seq
        |> Time.union
      | 3 ->
        let pick = if rng () mod 2 = 0 then `Whole else `Snd in
        let p1 = make_points ~rng ~min_year ~max_year_inc ~max_precision:6 in
        let p2 =
          make_points ~rng ~min_year ~max_year_inc
            ~max_precision:(Points.precision p1)
        in
        Time.bounded_intervals pick (make_pos_duration ~rng) p1 p2
      | 4 ->
        Time.chunk (make_chunking ~rng) (make_chunk_selector ~rng)
          (aux (new_height ~rng height))
      | _ -> failwith "Unexpected case"
      (* and aux_restricted height =
       *   if enable_extra_restrictions then
       *     Time.chunk `Disjoint_intervals
       *       Time.(take 100)
       *       (aux (new_height ~rng height))
       *   else aux (new_height ~rng height) *)
  in
  aux max_height
