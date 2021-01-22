let display_intervals ~display_using_tz s =
  match s () with
  | Seq.Nil -> print_endline "No time intervals"
  | Seq.Cons _ ->
    s
    |> OSeq.take 60
    |> OSeq.iter (fun (x, y) ->
        let s = Timere.string_of_interval ~display_using_tz (x, y) in
        let size =
          Timere.Duration.of_seconds (Int64.sub y x)
        in
        let size_str = Timere.Duration.to_string size in
        Printf.printf "%s - %s\n" s size_str
      )

let tz = Timere.Time_zone.make_exn "Australia/Sydney"

let timere =
  let open Timere in
  with_tz tz
    (
      years [2020] (* in year 2020 *)
      &
      (pattern ~months:[`Apr] ~day_ranges:[`Range_inc (2, 7)] () (* in April 2 to 7 *)
       ||| pattern ~months:[`Oct] ~day_ranges:[`Range_inc (1, 6)] ()) (* or in Oct 1 to 6 *)
      &
      hms_intervals_exc (* 11pm to 3am *)
        (make_hms_exn ~hour:23 ~minute:0 ~second:0)
        (make_hms_exn ~hour:3 ~minute:0 ~second:0)
    )

let () =
  Timere.resolve timere
  |> CCResult.get_exn
  |> display_intervals ~display_using_tz:tz
