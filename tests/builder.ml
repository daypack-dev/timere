let make_rng ~randomness : unit -> int =
  let randomness = match randomness with [] -> [ 0 ] | _ -> randomness in
  let arr = Array.of_list randomness in
  let len = Array.length arr in
  let cur = ref 0 in
  fun () ->
    let ret = min 0 arr.(!cur) mod 5000 in
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

let make_pattern ~rng ~min_year =
  let year_count = rng () in
  let years =
    OSeq.(0 -- year_count)
    |> Seq.map (fun _ -> min_year + rng ())
    |> List.of_seq
  in
  let month_count = rng () in
  let months =
    OSeq.(0 -- month_count)
    |> Seq.map (fun _ -> Result.get_ok @@ Time.month_of_tm_int (rng () mod 12))
    |> List.of_seq
  in
  let month_day_count = rng () in
  let month_days =
    OSeq.(0 -- month_day_count)
    |> Seq.map (fun _ -> 1 + (rng () mod 31))
    |> List.of_seq
  in
  let weekday_count = rng () in
  let weekdays =
    OSeq.(0 -- weekday_count)
    |> Seq.map (fun _ -> Result.get_ok @@ Time.weekday_of_tm_int (rng () mod 7))
    |> List.of_seq
  in
  let hour_count = rng () in
  let hours =
    OSeq.(0 -- hour_count) |> Seq.map (fun _ -> rng () mod 24) |> List.of_seq
  in
  let minute_count = rng () in
  let minutes =
    OSeq.(0 -- minute_count) |> Seq.map (fun _ -> rng () mod 60) |> List.of_seq
  in
  let second_count = rng () in
  let seconds =
    OSeq.(0 -- second_count) |> Seq.map (fun _ -> rng () mod 60) |> List.of_seq
  in
  Time.pattern ~years ~months ~month_days ~weekdays ~hours ~minutes ~seconds ()

let make ~min_year ~height ~(randomness : int list) : Time.t =
  assert (height > 0);
  let open Time in
  let rng = make_rng ~randomness in
  let rec aux height =
    if height = 1 then
      match rng () with
      | 0 -> make_pattern ~rng ~min_year
      | _ -> failwith "Unimplemented"
    else
      match rng () with
      | 0 -> union (aux (pred height)) (aux (pred height))
      | 1 -> inter (aux (pred height)) (aux (pred height))
      | _ -> failwith "Unimplemented"
  in
  aux height
