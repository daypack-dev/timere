open Date_time_components

module Search_param = struct
  type t = {
    search_using_tz_offset_s : int;
    start : Time.Date_time'.t;
    end_inc : Time.Date_time'.t;
  }

  let make ~search_using_tz_offset_s ((start, end_exc) : Time.Interval'.t) : t =
    let tz_of_date_time =
      Time_zone.make_offset_only_span
        (Span.make ~s:(Int64.of_int search_using_tz_offset_s) ())
    in
    {
      search_using_tz_offset_s;
      start =
        CCOpt.get_exn @@ Time.Date_time'.of_timestamp ~tz_of_date_time start;
      end_inc =
        CCOpt.get_exn
        @@ Time.Date_time'.of_timestamp ~tz_of_date_time (Span.pred end_exc);
    }
end

let failwith_unexpected_case (_ : 'a) = failwith "Unexpected case"

module Matching_seconds = struct
  let matching_second_ranges (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let cur_branch_search_start =
      Time.Date_time'.set_to_first_sec_ns cur_branch
    in
    let cur_branch_search_end_inc =
      Time.Date_time'.set_to_last_sec_ns cur_branch
    in
    let range_map_inc ~(cur_branch : Time.Date_time'.t) (x, y) =
      ( Time.Date_time'.set_to_first_ns { cur_branch with second = x },
        Time.Date_time'.set_to_last_ns { cur_branch with second = y } )
    in
    if Int_set.is_empty t.seconds then
      Seq.return
        (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
    else
      t.seconds
      |> Int_set.to_seq
      |> Time.Second_ranges.Of_seq.range_seq_of_seq
      |> Seq.map
        (Time.Range.map
           ~f_inc:(range_map_inc ~cur_branch)
           ~f_exc:failwith_unexpected_case)
end

module Matching_minutes = struct
  let get_cur_branch_search_start (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t =
    Time.Date_time'.set_to_first_min_sec_ns cur_branch

  let get_cur_branch_search_end_inc (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t =
    Time.Date_time'.set_to_last_min_sec_ns cur_branch

  let matching_minutes (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    if Int_set.is_empty t.minutes then
      Seq.map
        (fun minute -> { cur_branch with minute })
        OSeq.(
          cur_branch_search_start.minute -- cur_branch_search_end_inc.minute)
    else
      t.minutes
      |> Int_set.to_seq
      |> Seq.map (fun minute -> { cur_branch_search_start with minute })

  let matching_minute_ranges (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let range_map_inc ~(cur_branch : Time.Date_time'.t) (x, y) =
      ( Time.Date_time'.set_to_first_sec_ns { cur_branch with minute = x },
        Time.Date_time'.set_to_last_sec_ns { cur_branch with minute = y } )
    in
    if Int_set.is_empty t.minutes then
      Seq.return
        (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
    else
      t.minutes
      |> Int_set.to_seq
      |> Time.Minute_ranges.Of_seq.range_seq_of_seq
      |> Seq.map
        (Time.Range.map
           ~f_inc:(range_map_inc ~cur_branch)
           ~f_exc:failwith_unexpected_case)
end

module Matching_hours = struct
  let get_cur_branch_search_start (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t =
    Time.Date_time'.set_to_first_hour_min_sec_ns cur_branch

  let get_cur_branch_search_end_inc (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t =
    Time.Date_time'.set_to_last_hour_min_sec_ns cur_branch

  let matching_hours (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    if Int_set.is_empty t.hours then
      Seq.map
        (fun hour -> { cur_branch with hour })
        OSeq.(cur_branch_search_start.hour -- cur_branch_search_end_inc.hour)
    else
      t.hours
      |> Int_set.to_seq
      |> Seq.map (fun hour -> { cur_branch with hour })

  let matching_hour_ranges (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let range_map_inc ~(cur_branch : Time.Date_time'.t) (x, y) =
      ( Time.Date_time'.set_to_first_min_sec_ns { cur_branch with hour = x },
        Time.Date_time'.set_to_last_min_sec_ns { cur_branch with hour = y } )
    in
    if Int_set.is_empty t.hours then
      Seq.return
        (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
    else
      t.hours
      |> Int_set.to_seq
      |> Time.Hour_ranges.Of_seq.range_seq_of_seq
      |> Seq.map
        (Time.Range.map
           ~f_inc:(range_map_inc ~cur_branch)
           ~f_exc:failwith_unexpected_case)
end

module Matching_days = struct
  let get_cur_branch_search_start (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t =
    Time.Date_time'.set_to_first_day_hour_min_sec_ns cur_branch

  let get_cur_branch_search_end_inc (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t =
    Time.Date_time'.set_to_last_day_hour_min_sec_ns cur_branch

  let int_month_days_of_matching_weekdays ~(cur_branch : Time.Date_time'.t)
      (t : Pattern.t) : int Seq.t =
    let day_count =
      day_count_of_month ~year:cur_branch.year ~month:cur_branch.month
    in
    let day_start = 1 in
    let day_end_inc = day_count in
    if Weekday_set.is_empty t.weekdays then OSeq.(day_start -- day_end_inc)
    else
      OSeq.(day_start -- day_end_inc)
      |> Seq.filter (fun day ->
          match
            weekday_of_month_day ~year:cur_branch.year
              ~month:cur_branch.month ~day
          with
          | Some wday -> Weekday_set.mem wday t.weekdays
          | None -> false)

  let direct_matching_int_month_days ~(cur_branch : Time.Date_time'.t)
      (t : Pattern.t) : int Seq.t =
    let day_count =
      day_count_of_month ~year:cur_branch.year ~month:cur_branch.month
    in
    let day_start = 1 in
    let day_end_inc = day_count in
    if Int_set.is_empty t.month_days then OSeq.(day_start -- day_end_inc)
    else
      t.month_days
      |> Int_set.to_seq
      |> Seq.map (fun mday -> if mday < 0 then day_count + mday + 1 else mday)
      |> Seq.filter (fun mday -> mday <= day_end_inc)

  let matching_int_month_days ~(cur_branch : Time.Date_time'.t) (t : Pattern.t)
    : int Seq.t =
    let matching_month_days =
      direct_matching_int_month_days t ~cur_branch |> Int_set.of_seq
    in
    let month_days_of_matching_weekdays =
      int_month_days_of_matching_weekdays t ~cur_branch |> Int_set.of_seq
    in
    Int_set.inter matching_month_days month_days_of_matching_weekdays
    |> Int_set.to_seq

  let matching_days (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Seq.t =
    matching_int_month_days t ~cur_branch
    |> Seq.map (fun day -> { cur_branch with day })

  let matching_day_ranges (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let range_map_inc ~(cur_branch : Time.Date_time'.t) (x, y) =
      ( Time.Date_time'.set_to_first_hour_min_sec_ns { cur_branch with day = x },
        Time.Date_time'.set_to_last_hour_min_sec_ns { cur_branch with day = y }
      )
    in
    let f_inc = range_map_inc ~cur_branch in
    let f_exc = failwith_unexpected_case in
    match (Int_set.is_empty t.month_days, Weekday_set.is_empty t.weekdays) with
    | true, true ->
      Seq.return
        (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
    | true, false ->
      int_month_days_of_matching_weekdays ~cur_branch t
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map (Time.Range.map ~f_inc ~f_exc)
    | false, true ->
      direct_matching_int_month_days t ~cur_branch
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map (Time.Range.map ~f_inc ~f_exc)
    | false, false ->
      matching_int_month_days t ~cur_branch
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map (Time.Range.map ~f_inc ~f_exc)
end

module Matching_months = struct
  let get_cur_branch_search_start (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t =
    Time.Date_time'.set_to_first_month_day_hour_min_sec_ns cur_branch

  let get_cur_branch_search_end_inc (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t =
    Time.Date_time'.set_to_last_month_day_hour_min_sec_ns cur_branch

  let matching_months (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let month_start_int = human_int_of_month cur_branch_search_start.month in
    let month_end_inc_int =
      human_int_of_month cur_branch_search_end_inc.month
    in
    if Month_set.is_empty t.months then
      OSeq.(month_start_int -- month_end_inc_int)
      |> Seq.map (fun month -> month_of_human_int month |> CCOpt.get_exn)
      |> Seq.map (fun month -> { cur_branch_search_start with month })
    else
      t.months
      |> Month_set.to_seq
      |> Seq.map human_int_of_month
      |> Seq.filter (fun month ->
          month_start_int <= month && month <= month_end_inc_int)
      |> Seq.map (fun month -> month_of_human_int month |> CCOpt.get_exn)
      |> Seq.map (fun month -> { cur_branch_search_start with month })

  let matching_month_ranges (t : Pattern.t) (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let range_map_inc ~(cur_branch : Time.Date_time'.t) (x, y) =
      ( Time.Date_time'.set_to_first_day_hour_min_sec_ns
          { cur_branch with month = x },
        Time.Date_time'.set_to_last_day_hour_min_sec_ns
          { cur_branch with month = y } )
    in
    let month_start_int = human_int_of_month cur_branch_search_start.month in
    let month_end_inc_int =
      human_int_of_month cur_branch_search_end_inc.month
    in
    if Month_set.is_empty t.months then
      Seq.return
        (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
    else
      t.months
      |> Month_set.to_seq
      |> Seq.map human_int_of_month
      |> Seq.filter (fun month ->
          month_start_int <= month && month <= month_end_inc_int)
      |> Seq.map (fun month -> month_of_human_int month |> CCOpt.get_exn)
      |> Time.Month_ranges.Of_seq.range_seq_of_seq
      |> Seq.map
        (Time.Range.map
           ~f_inc:(range_map_inc ~cur_branch)
           ~f_exc:failwith_unexpected_case)
end

module Matching_years = struct
  let matching_years ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t) (t : Pattern.t) :
    Time.Date_time'.t Seq.t =
    if Int_set.is_empty t.years then
      OSeq.(overall_search_start.year -- overall_search_end_inc.year)
      |> Seq.map (fun year -> { overall_search_start with year })
    else
      t.years
      |> Int_set.to_seq
      |> Seq.filter (fun year ->
          overall_search_start.year <= year
          && year <= overall_search_end_inc.year)
      |> Seq.map (fun year -> { overall_search_start with year })

  let matching_year_ranges ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t) (t : Pattern.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let range_map_inc ~(overall_search_start : Time.Date_time'.t)
        ~(overall_search_end_inc : Time.Date_time'.t) (x, y) =
      let range_map_start =
        (* if x = overall_search_start.year then overall_search_start
         * else *)
        Time.Date_time'.set_to_first_month_day_hour_min_sec_ns
          { overall_search_start with year = x }
      in
      let range_map_end_inc =
        (* if y = overall_search_end_inc.year then overall_search_end_inc
         * else *)
        Time.Date_time'.set_to_last_month_day_hour_min_sec_ns
          { overall_search_end_inc with year = y }
      in
      (range_map_start, range_map_end_inc)
    in
    if Int_set.is_empty t.years then
      Seq.return (`Range_inc (overall_search_start, overall_search_end_inc))
    else
      t.years
      |> Int_set.to_seq
      |> Seq.filter (fun year ->
          overall_search_start.year <= year
          && year <= overall_search_end_inc.year)
      |> Time.Year_ranges.Of_seq.range_seq_of_seq
      |> Seq.map
        (Time.Range.map
           ~f_inc:
             (range_map_inc ~overall_search_start ~overall_search_end_inc)
           ~f_exc:failwith_unexpected_case)
end

type error = Pattern.error

let matching_date_time_ranges (search_param : Search_param.t) (t : Pattern.t) :
  Time.Date_time'.t Time.Range.range Seq.t =
  let overall_search_start =
    Time.Date_time'.set_to_first_month_day_hour_min_sec_ns search_param.start
  in
  let overall_search_end_inc =
    Time.Date_time'.set_to_last_month_day_hour_min_sec_ns search_param.end_inc
  in
  match
    ( Int_set.is_empty t.years,
      Month_set.is_empty t.months,
      Int_set.is_empty t.month_days,
      Weekday_set.is_empty t.weekdays,
      Int_set.is_empty t.hours,
      Int_set.is_empty t.minutes,
      Int_set.is_empty t.seconds )
  with
  | _years_is_empty, true, true, true, true, true, true ->
    Matching_years.matching_year_ranges ~overall_search_start
      ~overall_search_end_inc t
  | _years_is_empty, _months_is_empty, true, true, true, true, true ->
    Matching_years.matching_years ~overall_search_start
      ~overall_search_end_inc t
    |> Seq.flat_map (Matching_months.matching_month_ranges t)
  | ( _years_is_empty,
      _months_is_empty,
      _month_days_is_empty,
      _weekdays_is_empty,
      true,
      true,
      true ) ->
    Matching_years.matching_years ~overall_search_start
      ~overall_search_end_inc t
    |> Seq.flat_map (Matching_months.matching_months t)
    |> Seq.flat_map (Matching_days.matching_day_ranges t)
  | ( _years_is_empty,
      _months_is_empty,
      _month_days_is_empty,
      _weekdays_is_empty,
      _hours_is_empty,
      true,
      true ) ->
    Matching_years.matching_years ~overall_search_start
      ~overall_search_end_inc t
    |> Seq.flat_map (Matching_months.matching_months t)
    |> Seq.flat_map (Matching_days.matching_days t)
    |> Seq.flat_map (Matching_hours.matching_hour_ranges t)
  | ( _years_is_empty,
      _months_is_empty,
      _month_days_is_empty,
      _weekdays_is_empty,
      _hours_is_empty,
      _minutes_is_empty,
      true ) ->
    Matching_years.matching_years ~overall_search_start
      ~overall_search_end_inc t
    |> Seq.flat_map (Matching_months.matching_months t)
    |> Seq.flat_map (Matching_days.matching_days t)
    |> Seq.flat_map (Matching_hours.matching_hours t)
    |> Seq.flat_map (Matching_minutes.matching_minute_ranges t)
  | ( _years_is_empty,
      _months_is_empty,
      _month_days_is_empty,
      _weekdays_is_empty,
      _hours_is_empty,
      _minutes_is_empty,
      _seconds_is_empty ) ->
    Matching_years.matching_years ~overall_search_start
      ~overall_search_end_inc t
    |> Seq.flat_map (Matching_months.matching_months t)
    |> Seq.flat_map (Matching_days.matching_days t)
    |> Seq.flat_map (Matching_hours.matching_hours t)
    |> Seq.flat_map (Matching_minutes.matching_minutes t)
    |> Seq.flat_map (Matching_seconds.matching_second_ranges t)

let resolve (search_param : Search_param.t) (t : Pattern.t) :
  (Span.t * Span.t) Seq.t =
  let f (x, y) =
    let x = Time.Date_time'.to_timestamp_single x in
    let y = Time.Date_time'.to_timestamp_single y in
    (x, y)
  in
  matching_date_time_ranges search_param t
  |> Seq.map (Time.Range.map ~f_inc:f ~f_exc:failwith_unexpected_case)
  |> Seq.map (fun r ->
      match r with
      | `Range_inc (x, y) -> (x, Span.succ y)
      | `Range_exc _ -> failwith "Unexpected case")
  |> Time.Intervals.normalize ~skip_filter_invalid:true ~skip_sort:true
  |> Time.Intervals.Slice.slice
    ~start:(Time.Date_time'.to_timestamp_single search_param.start)
    ~end_exc:
      (Span.succ @@ Time.Date_time'.to_timestamp_single search_param.end_inc)
