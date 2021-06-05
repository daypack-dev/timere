open Fuzz_utils
open Span_set_utils

let timestamp_is_okay (tz : Timedesc.Time_zone.t) (pattern : Pattern.t)
    timestamp =
  let dt =
    CCOpt.get_exn_or "Expected successful construction of date time"
    @@ Timedesc.of_timestamp ~tz_of_date_time:tz timestamp
  in
  let weekday = Timedesc.weekday dt in
  let year_is_fine =
    Int_set.is_empty pattern.years
    || Int_set.mem (Timedesc.year dt) pattern.years
  in
  let month_is_fine =
    Int_set.is_empty pattern.months
    || Int_set.mem (Timedesc.month dt) pattern.months
  in
  let mday_is_fine =
    Int_set.is_empty pattern.month_days
    ||
    let day_count =
      Timedesc.Utils.day_count_of_month ~year:(Timedesc.year dt)
        ~month:(Timedesc.month dt)
    in
    pattern.month_days
    |> Int_set.to_seq
    |> Seq.map (fun mday -> if mday < 0 then day_count + mday + 1 else mday)
    |> OSeq.mem ~eq:( = ) (Timedesc.day dt)
  in
  let wday_is_fine =
    Weekday_set.is_empty pattern.weekdays
    || Weekday_set.mem weekday pattern.weekdays
  in
  let hour_is_fine =
    Int_set.is_empty pattern.hours
    || Int_set.mem (Timedesc.hour dt) pattern.hours
  in
  let minute_is_fine =
    Int_set.is_empty pattern.minutes
    || Int_set.mem (Timedesc.minute dt) pattern.minutes
  in
  let second_is_fine =
    Int_set.is_empty pattern.seconds
    || Int_set.mem (Timedesc.second dt) pattern.seconds
  in
  let ns_is_fine =
    Diet.Int.is_empty pattern.ns || Diet.Int.mem (Timedesc.ns dt) pattern.ns
  in
  year_is_fine
  && month_is_fine
  && mday_is_fine
  && wday_is_fine
  && hour_is_fine
  && minute_is_fine
  && second_is_fine
  && ns_is_fine

let () =
  Crowbar.add_test ~name:"pattern_resolution_is_sound"
    [ time_zone; search_space; pattern ] (fun tz search_space pattern ->
      let search_space_set =
        span_set_of_intervals @@ CCList.to_seq search_space
      in
      let s =
        Resolver.aux_pattern tz search_space pattern |> Resolver.normalize
      in
      let r =
        OSeq.for_all
          (fun (x, y) ->
            timestamp_is_okay tz pattern x
            && timestamp_is_okay tz pattern (Timedesc.Span.pred y)
            && not
                 (Span_set.mem y search_space_set
                 && timestamp_is_okay tz pattern y))
          s
      in
      if not r then
        Crowbar.fail
          (Fmt.str "tz: %s\nsearch_space: %a\npattern: %a\n"
             (Timedesc.Time_zone.name tz)
             (Fmt.list (Timedesc.Interval.pp ()))
             search_space CCSexp.pp
             (To_sexp.sexp_of_pattern pattern)))
