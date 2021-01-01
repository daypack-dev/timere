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
  let month = Result.get_ok @@ Time.month_of_tm_int (rng () mod 12) in
  let day = 1 + (rng () mod Time.day_count_of_month ~year ~month) in
  let hour = rng () mod 24 in
  let minute = rng () mod 60 in
  let second = rng () mod 60 in
  let available_time_zone_count = List.length Time_zone.available_time_zones in
  let tz =
    List.nth Time_zone.available_time_zones
      (rng () mod available_time_zone_count)
    |> Time_zone.make_exn
  in
  match Time.Date_time.make ~year ~month ~day ~hour ~minute ~second ~tz with
  | Error () ->
    Time.Date_time.make ~year ~month ~day ~hour ~minute ~second
      ~tz:Time_zone.utc
    |> Result.get_ok
  | Ok x -> x

let make_timestamp_intervals ~rng ~min_year ~max_year_inc =
  let len = min 5 (rng ()) in
  OSeq.(0 -- len)
  |> Seq.map (fun _ ->
      let start =
        make_date_time ~rng ~min_year ~max_year_inc
        |> Time.Date_time.to_timestamp
        |> Time.Date_time.min_of_timestamp_local_result
        |> Option.get
      in
      let end_exc = Int64.add start (Int64.of_int (rng ())) in
      (start, end_exc))
  |> List.of_seq
  |> List.sort_uniq Time.Interval.compare
  |> List.to_seq
  |> Time.of_interval_seq

let make_pattern ~rng ~min_year ~max_year_inc =
  let years =
    if rng () mod 2 = 0 then []
    else
      OSeq.(0 -- (Stdlib.min 5 (rng ())))
      |> Seq.map (fun _ -> min max_year_inc (min_year + rng ()))
      |> List.of_seq
  in
  let months =
    if rng () mod 2 = 0 then []
    else
      OSeq.(0 -- (Stdlib.min 5 (rng ())))
      |> Seq.map (fun _ ->
          Result.get_ok @@ Time.month_of_tm_int (rng () mod 12))
      |> List.of_seq
  in
  let month_days =
    if rng () mod 2 = 0 then []
    else
      OSeq.(0 -- (Stdlib.min 5 (rng ())))
      |> Seq.map (fun _ ->
          if rng () mod 2 = 0 then 1 + (rng () mod 31)
          else -(1 + (rng () mod 31)))
      |> List.of_seq
  in
  let weekdays =
    if rng () mod 2 = 0 then []
    else
      OSeq.(0 -- (Stdlib.min 5 (rng ())))
      |> Seq.map (fun _ ->
          Result.get_ok @@ Time.weekday_of_tm_int (rng () mod 7))
      |> List.of_seq
  in
  let hours =
    if rng () mod 2 = 0 then []
    else 
        OSeq.(0 -- (Stdlib.min 5 (rng ())))
         |> Seq.map (fun _ -> rng () mod 24) |> List.of_seq
  in
  let minutes =
    if rng () mod 2 = 0 then []
    else
      OSeq.(0 -- (Stdlib.min 5 (rng ())))
      |> Seq.map (fun _ -> rng () mod 60) |> List.of_seq
  in
  let seconds =
    if rng () mod 2 = 0 then []
    else
      OSeq.(0 -- (Stdlib.min 5 (rng ())))
      |> Seq.map (fun _ -> rng () mod 60) |> List.of_seq
  in
  Time.pattern ~years ~months ~month_days ~weekdays ~hours ~minutes ~seconds ()

let make_interval_inc ~rng ~min_year ~max_year_inc =
  let start_dt = make_date_time ~rng ~min_year ~max_year_inc in
  let start =
    Time.Date_time.to_timestamp start_dt
    |> Time.Date_time.min_of_timestamp_local_result
    |> Option.get
  in
  let end_inc = Int64.add start (Int64.of_int (rng ())) in
  Time.interval_inc start end_inc

let make_interval_exc ~rng ~min_year ~max_year_inc =
  let start_dt = make_date_time ~rng ~min_year ~max_year_inc in
  let start =
    Time.Date_time.to_timestamp start_dt
    |> Time.Date_time.min_of_timestamp_local_result
    |> Option.get
  in
  let end_exc = Int64.add start (Int64.of_int (rng ())) in
  Time.interval_exc start end_exc

let make_hms ~rng =
  Time.make_hms_exn ~hour:(rng () mod 24) ~minute:(rng () mod 60) ~second:(rng () mod 60)

let make_hms_interval_inc ~rng =
  Time.hms_interval_inc (make_hms ~rng) (make_hms ~rng)

let make_hms_interval_exc ~rng =
  Time.hms_interval_exc (make_hms ~rng) (make_hms ~rng)

let new_height ~rng height =
  assert (height > 1);
  let reduc = 1 + (rng () mod (height - 1)) in
  assert (reduc >= 1);
  height - reduc

let make_duration ~rng =
  let seconds =
    1 + rng ()
  in
  Duration.make ~seconds ()

let make_chunking ~rng : Time.chunking =
  match rng () mod 5 with
  | 0 -> `Disjoint_intervals
  | 1 -> `By_duration (make_duration ~rng)
  | 2 -> `By_duration_drop_partial (make_duration ~rng)
  | 3 -> `At_year_boundary
  | 4 -> `At_month_boundary
  | _ -> failwith "Unexpected case"

let make_chunk_selector ~rng : Time.chunked -> Time.chunked =
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
  aux Fun.id 5

let make_unary_op ~rng t =
  match rng () mod 6 with
  | 0 -> Time.not t
  | 1 -> Time.drop_n_points (rng () mod 5) t
  | 2 -> Time.take_n_points (rng () mod 5) t
  | 3 -> Time.shift (make_duration ~rng) t
  | 4 -> Time.lengthen (make_duration ~rng) t
  | 5 ->
    let available_time_zone_count =
      List.length Time_zone.available_time_zones
    in
    let tz =
      List.nth Time_zone.available_time_zones
        (rng () mod available_time_zone_count)
      |> Time_zone.make_exn
    in
    Time.with_tz tz t
  | _ -> failwith "Unexpected case"

let build ~min_year ~max_year_inc ~max_height ~max_branching
    ~(randomness : int list) : Time.t =
  let rng = make_rng ~randomness in
  let rec aux height =
    if height <= 1 then
      match rng () mod 8 with
      | 0 -> Time.empty
      | 1 -> Time.always
      | 2 -> make_timestamp_intervals ~rng ~min_year ~max_year_inc
      | 3 -> make_pattern ~rng ~min_year ~max_year_inc
      | 4 -> make_interval_inc ~rng ~min_year ~max_year_inc
      | 5 -> make_interval_exc ~rng ~min_year ~max_year_inc
      | 6 -> make_hms_interval_inc ~rng
      | 7 -> make_hms_interval_exc ~rng
      | _ -> failwith "Unexpected case"
    else
      match rng () mod 7 with
      | 0 -> make_unary_op ~rng (aux (new_height ~rng height))
      | 1 ->
        OSeq.(0 -- Stdlib.min max_branching (rng ()))
        |> Seq.map (fun _ -> aux (new_height ~rng height))
        |> List.of_seq
        |> Time.inter
      | 2 ->
        OSeq.(0 -- Stdlib.min max_branching (rng ()))
        |> Seq.map (fun _ -> aux (new_height ~rng height))
        |> List.of_seq
        |> Time.union
      | 3 ->
        Time.after (make_duration ~rng)
          (aux (new_height ~rng height))
          (aux (new_height ~rng height))
      | 4 ->
        Time.between_inc (make_duration ~rng)
          (aux (new_height ~rng height))
          (aux (new_height ~rng height))
      | 5 ->
        Time.between_exc (make_duration ~rng)
          (aux (new_height ~rng height))
          (aux (new_height ~rng height))
      | 6 ->
        Time.chunk (make_chunking ~rng) (make_chunk_selector ~rng)
          (aux (new_height ~rng height))
      (* | 3 ->
       *   OSeq.(0 -- Stdlib.min max_branching (rng ()))
       *   |> Seq.map (fun _ -> aux (new_height height))
       *   |> List.of_seq
       *   |> Time.round_robin_pick *)
      | _ -> failwith "Unexpected case"
  in
  aux max_height
