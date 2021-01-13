open Fuzz_utils
open Date_components

let () =
  Crowbar.add_test ~name:"pattern_resolution_is_sound" [ search_space; pattern ]
    (fun search_space pattern ->
       let tz = Time_zone.utc in
       let s =
         Resolver.aux_pattern tz search_space pattern
         |> Resolver.normalize
         |> Seq.flat_map (fun (a, b) -> Seq_utils.a_to_b_exc_int64 ~a ~b)
       in
       Crowbar.check
         (OSeq.for_all
            (fun timestamp ->
               let dt =
                 CCResult.get_exn
                 @@ Time.Date_time'.of_timestamp ~tz_of_date_time:tz timestamp
               in
               let weekday =
                 CCResult.get_exn
                 @@ weekday_of_month_day ~year:dt.year ~month:dt.month
                   ~mday:dt.day
               in
               let year_is_fine =
                 Int_set.is_empty pattern.years
                 || Int_set.mem dt.year pattern.years
               in
               let month_is_fine =
                 Month_set.is_empty pattern.months
                 || Month_set.mem dt.month pattern.months
               in
               let mday_is_fine =
                 Int_set.is_empty pattern.month_days
                 ||
                 let day_count =
                   day_count_of_month ~year:dt.year ~month:dt.month
                 in
                 pattern.month_days
                 |> Int_set.to_seq
                 |> Seq.map (fun mday ->
                     if mday < 0 then day_count + mday + 1 else mday)
                 |> OSeq.mem ~eq:( = ) dt.day
               in
               let wday_is_fine =
                 Weekday_set.is_empty pattern.weekdays
                 || Weekday_set.mem weekday pattern.weekdays
               in
               let hour_is_fine =
                 Int_set.is_empty pattern.hours
                 || Int_set.mem dt.hour pattern.hours
               in
               let minute_is_fine =
                 Int_set.is_empty pattern.minutes
                 || Int_set.mem dt.minute pattern.minutes
               in
               let second_is_fine =
                 Int_set.is_empty pattern.seconds
                 || Int_set.mem dt.second pattern.seconds
               in
               year_is_fine
               && month_is_fine
               && mday_is_fine
               && wday_is_fine
               && hour_is_fine
               && minute_is_fine
               && second_is_fine)
            s))
