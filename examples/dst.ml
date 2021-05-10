let display_intervals ~display_using_tz s =
  match s () with
  | Seq.Nil -> print_endline "No time intervals"
  | Seq.Cons _ ->
    s
    |> OSeq.take 60
    |> OSeq.iter (fun (x, y) ->
        let s = Timedesc.Interval.to_string ~display_using_tz (x, y) in
        let size =
          Timedesc.Span.sub y x
        in
        let size_str = Timedesc.Span.For_human.to_string size in
        Printf.printf "%s - %s\n" s size_str
      )

let tz = Timedesc.Time_zone.make_exn "Australia/Sydney"

let timere =
  let open Timere in
  with_tz tz
    (
      years [2020] (* in year 2020 *)
      &
      (pattern ~months:[4] ~day_ranges:[`Range_inc (2, 7)] () (* in April 2 to 7 *)
       ||| pattern ~months:[10] ~day_ranges:[`Range_inc (1, 6)] ()) (* or in Oct 1 to 6 *)
      &
      hms_intervals_exc (* 11pm to 3am *)
        (Hms.make_exn ~hour:23 ~minute:0 ~second:0)
        (Hms.make_exn ~hour:3 ~minute:0 ~second:0)
    )

let () =
  Timere.resolve timere
  |> CCResult.get_exn
  |> display_intervals ~display_using_tz:tz
