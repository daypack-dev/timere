let make_rng ~randomness : unit -> int =
  let randomness = match randomness with [] -> [ 0 ] | _ -> randomness in
  let arr = Array.of_list randomness in
  let len = Array.length arr in
  let cur = ref 0 in
  fun () ->
    let ret = max 0 (arr.(!cur) mod 5000) in
    cur := (!cur + 1) mod len;
    ret

let make_date_time ~rng ~min_year =
  let year = min_year + rng () in
  let month = Result.get_ok @@ Time.month_of_tm_int (rng () mod 12) in
  let day = 1 + (rng () mod Time.day_count_of_month ~year ~month) in
  let hour = rng () mod 24 in
  let minute = rng () mod 60 in
  let second = rng () mod 60 in
  Result.get_ok
  @@ Time.Date_time.make ~year ~month ~day ~hour ~minute ~second ~tz_offset_s:0

let make_timestamp_intervals ~rng ~min_year =
  let len = rng () in
  OSeq.(0 -- len)
  |> Seq.map (fun _ ->
      let start =
        make_date_time ~rng ~min_year |> Time.Date_time.to_timestamp
      in
      let end_exc = Int64.add start (Int64.of_int (rng ())) in
      (start, end_exc))
  |> List.of_seq
  |> List.sort_uniq Time.Interval.compare
  |> List.to_seq
  |> Time.of_intervals_seq

let make_pattern ~rng ~min_year =
  let years =
    OSeq.(0 -- rng ()) |> Seq.map (fun _ -> min_year + rng ()) |> List.of_seq
  in
  let months =
    OSeq.(0 -- rng ())
    |> Seq.map (fun _ -> Result.get_ok @@ Time.month_of_tm_int (rng () mod 12))
    |> List.of_seq
  in
  let month_days =
    OSeq.(0 -- rng ()) |> Seq.map (fun _ -> 1 + (rng () mod 31)) |> List.of_seq
  in
  let weekdays =
    OSeq.(0 -- rng ())
    |> Seq.map (fun _ -> Result.get_ok @@ Time.weekday_of_tm_int (rng () mod 7))
    |> List.of_seq
  in
  let hours =
    OSeq.(0 -- rng ()) |> Seq.map (fun _ -> rng () mod 24) |> List.of_seq
  in
  let minutes =
    OSeq.(0 -- rng ()) |> Seq.map (fun _ -> rng () mod 60) |> List.of_seq
  in
  let seconds =
    OSeq.(0 -- rng ()) |> Seq.map (fun _ -> rng () mod 60) |> List.of_seq
  in
  Time.pattern ~years ~months ~month_days ~weekdays ~hours ~minutes ~seconds ()

let make_branching ~rng ~min_year =
  let years =
    OSeq.(0 -- rng ())
    |> Seq.map (fun _ ->
        let start = min_year + rng () in
        let end_inc = start + rng () in
        `Range_inc (start, end_inc))
    |> List.of_seq
  in
  let months =
    OSeq.(0 -- rng ())
    |> Seq.map (fun _ ->
        let start_int = rng () mod 12 in
        let end_inc_int = min 11 (start_int + rng ()) in
        let start = Result.get_ok @@ Time.month_of_tm_int start_int in
        let end_inc = Result.get_ok @@ Time.month_of_tm_int end_inc_int in
        `Range_inc (start, end_inc))
    |> List.of_seq
  in
  let days =
    Time.Month_days
      ( OSeq.(0 -- rng ())
        |> Seq.map (fun _ ->
            let rand = rng () mod 2 in
            if rand = 0 then
              let start = 1 + (rng () mod 31) in
              let end_inc = (min 31 (start + rng ())) in
              `Range_inc (start, end_inc)
            else
                let start = - (1 + (rng () mod 31)) in
                let end_inc = min (-1) (start + rng ()) in
                `Range_inc (start, end_inc)
          )
        |> List.of_seq )
  in
  let hmss =
    OSeq.(0 -- rng ())
    |> Seq.map (fun _ ->
        let start_int = rng () mod (24 * 60 * 60) in
        let end_inc_int = min (24 * 60 * 60) (start_int + rng ()) in
        let start = Time.hms_of_second_of_day start_int in
        let end_inc = Time.hms_of_second_of_day end_inc_int in
        `Range_inc (start, end_inc))
    |> List.of_seq
  in
  Time.branching ~allow_out_of_range_month_day:true ~years ~months ~days ~hmss
    ()

let make_interval_inc ~rng ~min_year =
  let start_dt = make_date_time ~rng ~min_year in
  let start = Time.Date_time.to_timestamp start_dt in
  let end_inc = Int64.add start (Int64.of_int (rng ())) in
  Time.interval_inc start end_inc

let make_interval_exc ~rng ~min_year =
  let start_dt = make_date_time ~rng ~min_year in
  let start = Time.Date_time.to_timestamp start_dt in
  let end_exc = Int64.add start (Int64.of_int (rng ())) in
  Time.interval_exc start end_exc

let make_unary_op ~rng t =
  match rng () mod 9 with
  | 0 -> Time.not t
  | 1 -> Time.skip_n_points (rng ()) t
  | 2 -> Time.skip_n (rng ()) t
  | 3 -> Time.take_n_points (rng ()) t
  | 4 -> Time.take_n (rng ()) t
  | 5 ->
    Time.chunk
      (Result.get_ok @@ Duration.of_seconds @@ Int64.of_int (rng ()))
      t
  | 6 -> Time.shift (Result.get_ok @@ Duration.make ~seconds:(rng ()) ()) t
  | 7 -> Time.lengthen (Result.get_ok @@ Duration.make ~seconds:(rng ()) ()) t
  | 8 -> Time.change_tz_offset_s (rng ()) t
  | _ -> failwith "Unexpected case"

let make_binary_op ~rng t1 t2 =
  match rng () mod 2 with
  | 0 -> Time.union t1 t2
  | 1 -> Time.inter t1 t2
  | _ -> failwith "Unexpected case"

let make ~min_year ~height ~max_branching ~(randomness : int list) : Time.t =
  if height <= 0 then invalid_arg "make";
  let rng = make_rng ~randomness in
  let rec aux height =
    if height = 1 then
      match rng () mod 5 with
      | 0 -> make_timestamp_intervals ~rng ~min_year
      | 1 -> make_pattern ~rng ~min_year
      | 2 -> make_branching ~rng ~min_year
      | 3 -> make_interval_inc ~rng ~min_year
      | 4 -> make_interval_exc ~rng ~min_year
      | _ -> failwith "Unexpected case"
    else
      match rng () mod 4 with
      | 0 -> make_unary_op ~rng (aux (pred height))
      | 1 -> make_binary_op ~rng (aux (pred height)) (aux (pred height))
      | 2 ->
        OSeq.(0 -- Stdlib.min max_branching (rng ()))
        |> Seq.map (fun _ -> aux (pred height))
        |> List.of_seq
        |> Time.round_robin_pick
      | 3 ->
        OSeq.(0 -- Stdlib.min max_branching (rng ()))
        |> Seq.map (fun _ -> aux (pred height))
        |> List.of_seq
        |> Time.merge
      | _ -> failwith "Unexpected case"
  in
  aux height
