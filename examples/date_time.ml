let () =
  let tz =
    Timere.Time_zone.make_exn "Australia/Sydney"
  in
  print_endline (Timere.Time_zone.name tz);

  print_endline "=====";

  let ordinary_dt =
    (* very ordinary date time with no funny business *)
    Timere.Date_time.make_exn ~tz ~year:2021 ~month:`Mar ~day:1 ~hour:13 ~minute:0 ~second:0 ()
  in
  Fmt.pr "%a@." (Timere.Date_time.pp ()) ordinary_dt;
  let ordinary_timestamp =
    (* since it is ordinary, we can get a single/unique timestamp out of it *)
    Timere.Date_time.to_timestamp_single ordinary_dt
  in
  Fmt.pr "%a@." (Timere.Timestamp.pp ~display_using_tz:tz ()) ordinary_timestamp;

  print_endline "=====";

  let ambiguous_dt =
    (* this date time is ambiguous due to DST ending *)
    Timere.Date_time.make_exn ~tz ~year:2021 ~month:`Apr ~day:4 ~hour:2 ~minute:30 ~second:0 ()
  in
  Fmt.pr "%a@." (Timere.Date_time.pp ()) ambiguous_dt;
  (match Timere.Date_time.to_timestamp ambiguous_dt with
   | `Single _ -> failwith "Unexpected case"
   | `Ambiguous (x, y) ->
     Fmt.pr "%a@." (Timere.Timestamp.pp ~display_using_tz:tz ()) x;
     Fmt.pr "%a@." (Timere.Timestamp.pp ~display_using_tz:tz ()) y;
  );

  print_endline "=====";

  let non_existent_dt =
    (* this date time doesn't exist due to DST starting *)
    Timere.Date_time.make ~tz ~year:2021 ~month:`Oct ~day:3 ~hour:2 ~minute:30 ~second:0 ()
  in
  match non_existent_dt with
  | None -> print_endline "Failed to construct date time"
  | Some _ -> failwith "Unexpected case"
