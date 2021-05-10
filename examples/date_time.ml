let () =
  let tz =
    Timedesc.Time_zone.make_exn "Australia/Sydney"
  in
  print_endline (Timedesc.Time_zone.name tz);

  print_endline "=====";

  let ordinary_dt =
    (* very ordinary date time with no funny business *)
    Timedesc.make_exn ~tz ~year:2021 ~month:5 ~day:1 ~hour:13 ~minute:0 ~second:0 ()
  in
  Fmt.pr "%a@." (Timedesc.pp ()) ordinary_dt;
  let ordinary_timestamp =
    (* since it is ordinary, we can get a single/unique timestamp out of it *)
    Timedesc.to_timestamp_single ordinary_dt
  in
  Fmt.pr "%a@." (Timedesc.Timestamp.pp ~display_using_tz:tz ()) ordinary_timestamp;

  print_endline "=====";

  let ambiguous_dt =
    (* this date time is ambiguous due to DST ending *)
    Timedesc.make_exn ~tz ~year:2021 ~month:4 ~day:4 ~hour:2 ~minute:30 ~second:0 ()
  in
  Fmt.pr "%a@." (Timedesc.pp ()) ambiguous_dt;
  (match Timedesc.to_timestamp ambiguous_dt with
   | `Single _ -> failwith "Unexpected case"
   | `Ambiguous (x, y) ->
     Fmt.pr "%a@." (Timedesc.Timestamp.pp ~display_using_tz:tz ()) x;
     Fmt.pr "%a@." (Timedesc.Timestamp.pp ~display_using_tz:tz ()) y;
  );

  print_endline "=====";

  let non_existent_dt =
    (* this date time doesn't exist due to DST starting *)
    Timedesc.make ~tz ~year:2021 ~month:10 ~day:3 ~hour:2 ~minute:30 ~second:0 ()
  in
  match non_existent_dt with
  | Error _ -> print_endline "Failed to construct date time"
  | Ok _ -> failwith "Unexpected case"
