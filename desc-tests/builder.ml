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

let make_duration ~rng =
  let sign = if rng () mod 2 = 0 then `Pos else `Neg in
  let seconds = rng () in
  let ns = 1 + rng () in
  Timedesc.Span.For_human.make_exn ~sign ~seconds ~ns ()

let make_pos_duration ~rng =
  let seconds = rng () in
  let ns = 1 + rng () in
  Timedesc.Span.For_human.make_exn ~seconds ~ns ()
