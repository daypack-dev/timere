module Search_param = struct
  type t = {
    search_using_tz_offset_s : Time.tz_offset_s;
    start : Time.Date_time.t;
    end_inc : Time.Date_time.t;
  }

  let make ~search_using_tz_offset_s ((start, end_exc) : Time.Interval.t) : t =
    {
      search_using_tz_offset_s;
      start =
        Result.get_ok
        @@ Time.Date_time.of_timestamp
          ~tz_offset_s_of_date_time:search_using_tz_offset_s start;
      end_inc =
        Result.get_ok
        @@ Time.Date_time.of_timestamp
          ~tz_offset_s_of_date_time:search_using_tz_offset_s
          (Int64.pred end_exc);
    }
end

module Resolve_pattern = struct
  module Matching_seconds = struct
    let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_start.year
        && cur_branch.month = overall_search_start.month
        && cur_branch.day = overall_search_start.day
        && cur_branch.hour = overall_search_start.hour
        && cur_branch.minute = overall_search_start.minute
      then overall_search_start
      else Time.Date_time.set_to_first_sec cur_branch

    let matching_seconds ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.t) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      if Int_set.is_empty t.seconds then
        Seq.map
          (fun second -> { cur_branch with second })
          OSeq.(cur_branch_search_start.second --^ 60)
      else
        t.seconds
        |> Int_set.to_seq
        |> Seq.filter (fun second ->
            cur_branch_search_start.second <= second && second < 60)
        |> Seq.map (fun second -> { cur_branch with second })

    let matching_second_ranges (t : Time.Pattern.t)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_start ~(cur_branch_search_start : Time.Date_time.t) x =
        if x = cur_branch_search_start.second then cur_branch_search_start
        else { cur_branch_search_start with second = x }
      in
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          { cur_branch_search_start with second = y } )
      in
      let range_map_exc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          { cur_branch_search_start with second = y } )
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      if Int_set.is_empty t.seconds then
        Seq.return
          (`Range_inc
             (cur_branch_search_start, Time.Date_time.set_to_last_sec cur_branch))
      else
        t.seconds
        |> Int_set.to_seq
        |> Time.Second_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:(range_map_inc ~cur_branch_search_start)
             ~f_exc:(range_map_exc ~cur_branch_search_start))
  end

  module Matching_minutes = struct
    let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_start.year
        && cur_branch.month = overall_search_start.month
        && cur_branch.day = overall_search_start.day
        && cur_branch.hour = overall_search_start.hour
      then overall_search_start
      else Time.Date_time.set_to_first_min_sec cur_branch

    let matching_minutes ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.t) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      if Int_set.is_empty t.minutes then
        Seq.map
          (fun minute -> { cur_branch with minute })
          OSeq.(cur_branch_search_start.minute --^ 60)
      else
        t.minutes
        |> Int_set.to_seq
        |> Seq.filter (fun minute ->
            cur_branch_search_start.minute <= minute && minute < 60)
        |> Seq.map (fun minute -> { cur_branch_search_start with minute })

    let matching_minute_ranges ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.t) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_start ~(cur_branch_search_start : Time.Date_time.t) x =
        if x = cur_branch_search_start.minute then cur_branch_search_start
        else
          Time.Date_time.set_to_first_sec
            { cur_branch_search_start with minute = x }
      in
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          Time.Date_time.set_to_last_sec
            { cur_branch_search_start with minute = y } )
      in
      let range_map_exc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          Time.Date_time.set_to_first_sec
            { cur_branch_search_start with minute = y } )
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      if Int_set.is_empty t.minutes then
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_min_sec cur_branch_search_start ))
      else
        t.minutes
        |> Int_set.to_seq
        |> Seq.filter (fun min -> cur_branch_search_start.minute <= min)
        |> Time.Minute_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:(range_map_inc ~cur_branch_search_start)
             ~f_exc:(range_map_exc ~cur_branch_search_start))
  end

  module Matching_hours = struct
    let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_start.year
        && cur_branch.month = overall_search_start.month
        && cur_branch.day = overall_search_start.day
      then overall_search_start
      else Time.Date_time.set_to_first_hour_min_sec cur_branch

    let matching_hours (t : Time.Pattern.t)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      if Int_set.is_empty t.hours then
        Seq.map
          (fun hour -> { cur_branch with hour })
          OSeq.(cur_branch_search_start.hour --^ 24)
      else
        t.hours
        |> Int_set.to_seq
        |> Seq.filter (fun hour ->
            cur_branch_search_start.hour <= hour && hour < 24)
        |> Seq.map (fun hour -> { cur_branch with hour })

    let matching_hour_ranges ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.t) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_start ~(cur_branch_search_start : Time.Date_time.t) x =
        if x = cur_branch_search_start.hour then cur_branch_search_start
        else
          Time.Date_time.set_to_first_min_sec
            { cur_branch_search_start with hour = x }
      in
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          Time.Date_time.set_to_last_min_sec
            { cur_branch_search_start with hour = y } )
      in
      let range_map_exc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          Time.Date_time.set_to_first_min_sec
            { cur_branch_search_start with hour = y } )
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      if Int_set.is_empty t.hours then
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_hour_min_sec cur_branch ))
      else
        t.hours
        |> Int_set.to_seq
        |> Seq.filter (fun hour ->
            cur_branch_search_start.hour <= hour && hour < 24)
        |> Time.Hour_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:(range_map_inc ~cur_branch_search_start)
             ~f_exc:(range_map_exc ~cur_branch_search_start))
  end

  module Matching_days = struct
    let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_start.year
        && cur_branch.month = overall_search_start.month
      then overall_search_start
      else Time.Date_time.set_to_first_day_hour_min_sec cur_branch

    let int_month_days_of_matching_weekdays
        ~(cur_branch_search_start : Time.Date_time.t) (t : Time.Pattern.t) :
      int Seq.t =
      let day_count =
        Time.day_count_of_month ~year:cur_branch_search_start.year
          ~month:cur_branch_search_start.month
      in
      if Time.Weekday_set.is_empty t.weekdays then
        OSeq.(cur_branch_search_start.day -- day_count)
      else
        OSeq.(cur_branch_search_start.day -- day_count)
        |> Seq.filter (fun mday ->
            match
              Time.weekday_of_month_day ~year:cur_branch_search_start.year
                ~month:cur_branch_search_start.month ~mday
            with
            | Ok wday -> Time.Weekday_set.mem wday t.weekdays
            | Error () -> false)

    let direct_matching_int_month_days
        ~(cur_branch_search_start : Time.Date_time.t) (t : Time.Pattern.t) :
      int Seq.t =
      let day_count =
        Time.day_count_of_month ~year:cur_branch_search_start.year
          ~month:cur_branch_search_start.month
      in
      if Int_set.is_empty t.month_days then
        OSeq.(cur_branch_search_start.day -- day_count)
      else
        t.month_days
        |> Int_set.to_seq
        |> Seq.map (fun mday -> if mday < 0 then day_count + mday + 1 else mday)
        |> Seq.filter (fun mday ->
            cur_branch_search_start.day <= mday && mday <= day_count)

    let matching_int_month_days ~(cur_branch_search_start : Time.Date_time.t)
        (t : Time.Pattern.t) : int Seq.t =
      let matching_month_days =
        direct_matching_int_month_days t ~cur_branch_search_start
        |> Int_set.of_seq
      in
      let month_days_of_matching_weekdays =
        int_month_days_of_matching_weekdays t ~cur_branch_search_start
        |> Int_set.of_seq
      in
      OSeq.(1 -- 31)
      |> Seq.filter (fun mday ->
          Int_set.mem mday matching_month_days
          && Int_set.mem mday month_days_of_matching_weekdays)

    let matching_days ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.t) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      matching_int_month_days t ~cur_branch_search_start
      |> Seq.map (fun day -> { cur_branch_search_start with day })

    let matching_day_ranges ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.t) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_start ~(cur_branch_search_start : Time.Date_time.t) x =
        if x = cur_branch_search_start.day then cur_branch_search_start
        else
          Time.Date_time.set_to_first_hour_min_sec
            { cur_branch_search_start with day = x }
      in
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          Time.Date_time.set_to_last_hour_min_sec
            { cur_branch_search_start with day = y } )
      in
      let range_map_exc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          Time.Date_time.set_to_first_hour_min_sec
            { cur_branch_search_start with day = y } )
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let f_inc = range_map_inc ~cur_branch_search_start in
      let f_exc = range_map_exc ~cur_branch_search_start in
      match
        (Int_set.is_empty t.month_days, Time.Weekday_set.is_empty t.weekdays)
      with
      | true, true ->
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_day_hour_min_sec cur_branch ))
      | true, false ->
        int_month_days_of_matching_weekdays t ~cur_branch_search_start
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
      | false, true ->
        direct_matching_int_month_days t ~cur_branch_search_start
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
      | false, false ->
        matching_int_month_days t ~cur_branch_search_start
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
  end

  module Matching_months = struct
    let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if cur_branch.year = overall_search_start.year then overall_search_start
      else Time.Date_time.set_to_first_month_day_hour_min_sec cur_branch

    let matching_months (t : Time.Pattern.t)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let start_month_int =
        Time.human_int_of_month cur_branch_search_start.month
      in
      if Time.Month_set.is_empty t.months then
        OSeq.(start_month_int -- 12)
        |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
        |> Seq.map (fun month -> { cur_branch_search_start with month })
      else
        t.months
        |> Time.Month_set.to_seq
        |> Seq.map Time.human_int_of_month
        |> Seq.filter (fun month -> start_month_int <= month && month <= 12)
        |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
        |> Seq.map (fun month -> { cur_branch_search_start with month })

    let matching_month_ranges (t : Time.Pattern.t)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_start ~(cur_branch_search_start : Time.Date_time.t) x =
        if x = cur_branch_search_start.month then cur_branch_search_start
        else
          Time.Date_time.set_to_first_day_hour_min_sec
            { cur_branch_search_start with month = x }
      in
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          Time.Date_time.set_to_last_day_hour_min_sec
            { cur_branch_search_start with month = y } )
      in
      let range_map_exc ~(cur_branch_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~cur_branch_search_start x,
          Time.Date_time.set_to_first_day_hour_min_sec
            { cur_branch_search_start with month = y } )
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let start_month_int =
        Time.human_int_of_month cur_branch_search_start.month
      in
      if Time.Month_set.is_empty t.months then
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_month_day_hour_min_sec
                 cur_branch_search_start ))
      else
        t.months
        |> Time.Month_set.to_seq
        |> Seq.map Time.human_int_of_month
        |> Seq.filter (fun month -> start_month_int <= month && month <= 12)
        |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
        |> Time.Month_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:(range_map_inc ~cur_branch_search_start)
             ~f_exc:(range_map_exc ~cur_branch_search_start))
  end

  module Matching_years = struct
    let matching_years ~(overall_search_start : Time.Date_time.t) ~end_year_inc
        (t : Time.Pattern.t) : Time.Date_time.t Seq.t =
      if Int_set.is_empty t.years then
        OSeq.(overall_search_start.year -- end_year_inc)
        |> Seq.map (fun year -> { overall_search_start with year })
      else
        t.years
        |> Int_set.to_seq
        |> Seq.filter (fun year ->
            overall_search_start.year <= year && year <= end_year_inc)
        |> Seq.map (fun year -> { overall_search_start with year })

    let matching_year_ranges ~(overall_search_start : Time.Date_time.t)
        ~end_year_inc (t : Time.Pattern.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_start ~(overall_search_start : Time.Date_time.t) x =
        if x = overall_search_start.year then overall_search_start
        else
          Time.Date_time.set_to_first_month_day_hour_min_sec
            { overall_search_start with year = x }
      in
      let range_map_inc ~(overall_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~overall_search_start x,
          Time.Date_time.set_to_last_month_day_hour_min_sec
            { overall_search_start with year = y } )
      in
      let range_map_exc ~(overall_search_start : Time.Date_time.t) (x, y) =
        ( range_map_start ~overall_search_start x,
          Time.Date_time.set_to_last_month_day_hour_min_sec
            { overall_search_start with year = y } )
      in
      if Int_set.is_empty t.years then
        Seq.return
          (`Range_inc
             ( overall_search_start,
               Time.Date_time.set_to_last_month_day_hour_min_sec
                 { overall_search_start with year = end_year_inc } ))
      else
        t.years
        |> Int_set.to_seq
        |> Seq.filter (fun year ->
            overall_search_start.year <= year && year <= end_year_inc)
        |> Time.Year_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:(range_map_inc ~overall_search_start)
             ~f_exc:(range_map_exc ~overall_search_start))
  end

  let date_time_range_seq_of_timestamps ~search_using_tz_offset_s
      (s : int64 Seq.t) : Time.Date_time.t Time.Range.range Seq.t =
    let f (x, y) =
      ( Time.Date_time.of_timestamp
          ~tz_offset_s_of_date_time:search_using_tz_offset_s x,
        Time.Date_time.of_timestamp
          ~tz_offset_s_of_date_time:search_using_tz_offset_s y )
    in
    s
    |> Time.Ranges.Of_seq.range_seq_of_seq ~modulo:None
      ~to_int64:(fun x -> x)
      ~of_int64:(fun x -> x)
    |> Seq.map (Time.Range.map ~f_inc:f ~f_exc:f)
    |> Seq.filter_map Time.Range_utils.result_range_get

  type error = Time.Pattern.error

  let matching_date_times (search_param : Search_param.t) (pat : Time.Pattern.t)
    : Time.Date_time.t Seq.t =
    let overall_search_start = search_param.start in
    Matching_years.matching_years ~overall_search_start
      ~end_year_inc:search_param.end_inc.year pat
    |> Seq.flat_map (Matching_months.matching_months pat ~overall_search_start)
    |> Seq.flat_map (Matching_days.matching_days pat ~overall_search_start)
    |> Seq.flat_map (Matching_hours.matching_hours pat ~overall_search_start)
    |> Seq.flat_map
      (Matching_minutes.matching_minutes pat ~overall_search_start)
    |> Seq.flat_map
      (Matching_seconds.matching_seconds pat ~overall_search_start)

  let matching_date_time_ranges (search_param : Search_param.t)
      (t : Time.Pattern.t) : Time.Date_time.t Time.Range.range Seq.t =
    let overall_search_start = search_param.start in
    let end_year_inc = search_param.end_inc.year in
    match
      ( Int_set.is_empty t.years,
        Time.Month_set.is_empty t.months,
        Int_set.is_empty t.month_days,
        Time.Weekday_set.is_empty t.weekdays,
        Int_set.is_empty t.hours,
        Int_set.is_empty t.minutes,
        Int_set.is_empty t.seconds )
    with
    | _years_is_empty, true, true, true, true, true, true ->
      Matching_years.matching_year_ranges ~overall_search_start ~end_year_inc
        t
    | _years_is_empty, _months_is_empty, true, true, true, true, true ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_month_ranges t ~overall_search_start)
    | ( _years_is_empty,
        _months_is_empty,
        _month_days_is_empty,
        _weekdays_is_empty,
        true,
        true,
        true ) ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map
        (Matching_days.matching_day_ranges t ~overall_search_start)
    | ( _years_is_empty,
        _months_is_empty,
        _month_days_is_empty,
        _weekdays_is_empty,
        _hours_is_empty,
        true,
        true ) ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map (Matching_days.matching_days t ~overall_search_start)
      |> Seq.flat_map
        (Matching_hours.matching_hour_ranges t ~overall_search_start)
    | ( _years_is_empty,
        _months_is_empty,
        _month_days_is_empty,
        _weekdays_is_empty,
        _hours_is_empty,
        _minutes_is_empty,
        true ) ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map (Matching_days.matching_days t ~overall_search_start)
      |> Seq.flat_map (Matching_hours.matching_hours t ~overall_search_start)
      |> Seq.flat_map
        (Matching_minutes.matching_minute_ranges t ~overall_search_start)
    | ( _years_is_empty,
        _months_is_empty,
        _month_days_is_empty,
        _weekdays_is_empty,
        _hours_is_empty,
        _minutes_is_empty,
        _seconds_is_empty ) ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map (Matching_days.matching_days t ~overall_search_start)
      |> Seq.flat_map (Matching_hours.matching_hours t ~overall_search_start)
      |> Seq.flat_map
        (Matching_minutes.matching_minutes t ~overall_search_start)
      |> Seq.flat_map
        (Matching_seconds.matching_second_ranges t ~overall_search_start)

  let matching_intervals (search_param : Search_param.t) (t : Time.Pattern.t) :
    (int64 * int64) Seq.t =
    let f (x, y) =
      (Time.Date_time.to_timestamp x, Time.Date_time.to_timestamp y)
    in
    matching_date_time_ranges search_param t
    |> Seq.map (Time.Range.map ~f_inc:f ~f_exc:f)
    |> Seq.map (fun r ->
        match r with
        | `Range_inc (x, y) -> (x, Int64.succ y)
        | `Range_exc (x, y) -> (x, y))

  let matching_intervals_round_robin_non_decreasing
      (search_param : Search_param.t) (l : Time.Pattern.t list) :
    ((int64 * int64) list Seq.t, error) result =
    let l = List.map (matching_intervals search_param) l in
    l
    |> Time.Intervals.Round_robin.collect_round_robin_non_decreasing
      ~skip_check:true
    |> OSeq.take_while (List.for_all Option.is_some)
    |> Seq.map (List.map Option.get)
    |> Result.ok

  let matching_intervals_round_robin_non_decreasing_flat
      (search_param : Search_param.t) (l : Time.Pattern.t list) :
    ((int64 * int64) Seq.t, error) result =
    matching_intervals_round_robin_non_decreasing search_param l
    |> Result.map (Seq.flat_map List.to_seq)

  let next_match_date_time (search_param : Search_param.t) (t : Time.Pattern.t)
    : Time.Date_time.t option =
    let s = matching_date_times search_param t in
    match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x

  let next_match_timestamp (search_param : Search_param.t) (t : Time.Pattern.t)
    : int64 option =
    match next_match_date_time search_param t with
    | None -> None
    | Some x -> Some (Time.Date_time.to_timestamp x)

  let next_match_interval (search_param : Search_param.t) (t : Time.Pattern.t) :
    (int64 * int64) option =
    let s = matching_intervals search_param t in
    match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x
end

let rec get_search_space (time : Time.t) : Time.Interval.t list =
  let open Time in
  match time with
  | All -> default_search_space
  | Empty -> []
  | Timestamp_interval_seq (s, _) -> s
  | Pattern (s, _) -> s
  | Branching (s, _) -> s
  | Recur (s, _) -> s
  | Unary_op (s, _, _) -> s
  | Interval_exc (s, _, _) -> s
  | Interval_inc (s, _, _) -> s
  | Round_robin_pick_list (s, _) -> s
  | Inter_seq (s, _) -> s
  | Union_seq (s, _) -> s
  | After (s, _, _) -> s
  | Between_inc (s, _, _) -> s
  | Between_exc (s, _, _) -> s
  | Unchunk c ->
    get_search_space_chunked c

and get_search_space_chunked (chunked : Time.chunked) =
  let open Time in
  match chunked with
  | Unary_op_on_t (_, t) -> get_search_space t
  | Unary_op_on_chunked (_, c) -> get_search_space_chunked c

let set_search_space space (time : Time.t) : Time.t =
  let open Time in
  match time with
  | All -> All
  | Empty -> Empty
  | Timestamp_interval_seq (_, x) -> Timestamp_interval_seq (space, x)
  | Pattern (_, x) -> Pattern (space, x)
  | Branching (_, x) -> Branching (space, x)
  | Recur (_, x) -> Recur (space, x)
  | Unary_op (_, op, x) -> Unary_op (space, op, x)
  | Interval_exc (_, x, y) -> Interval_exc (space, x, y)
  | Interval_inc (_, x, y) -> Interval_inc (space, x, y)
  | Round_robin_pick_list (_, x) -> Round_robin_pick_list (space, x)
  | Inter_seq (_, x) -> Inter_seq (space, x)
  | Union_seq (_, x) -> Union_seq (space, x)
  | After (_, x, y) -> After (space, x, y)
  | Between_inc (_, x, y) -> Between_inc (space, x, y)
  | Between_exc (_, x, y) -> Between_exc (space, x, y)
  | Unchunk c -> Unchunk c

let search_space_of_year_range tz_offset_s year_range =
  let open Time in
  match year_range with
  | `Range_inc (start, end_inc) ->
    ( Date_time.set_to_first_month_day_hour_min_sec
        { Date_time.min with year = start; tz_offset_s }
      |> Date_time.to_timestamp,
      Date_time.set_to_last_month_day_hour_min_sec
        { Date_time.min with year = end_inc; tz_offset_s }
      |> Date_time.to_timestamp
      |> Int64.succ )
  | `Range_exc (start, end_exc) ->
    ( Date_time.set_to_first_month_day_hour_min_sec
        { Date_time.min with year = start; tz_offset_s }
      |> Date_time.to_timestamp,
      Date_time.set_to_last_month_day_hour_min_sec
        { Date_time.min with year = end_exc; tz_offset_s }
      |> Date_time.to_timestamp )

let search_space_of_year tz_offset_s year =
  search_space_of_year_range tz_offset_s (`Range_inc (year, year))

let empty_search_space = []

let propagate_search_space_bottom_up default_tz_offset_s (time : Time.t) :
  Time.t =
  let open Time in
  let rec aux tz_offset_s time =
    match time with
    | All -> All
    | Empty -> Empty
    | Timestamp_interval_seq (_, s) -> (
        match s () with
        | Seq.Nil -> time
        | Seq.Cons ((start, _), _) ->
          Timestamp_interval_seq ([ (start, default_search_space_end_exc) ], s)
      )
    | Pattern (_, pat) ->
      if Int_set.is_empty pat.years then time
      else
        let space =
          pat.years
          |> Int_set.to_seq
          |> Seq.map (search_space_of_year tz_offset_s)
          |> List.of_seq
        in
        Pattern (space, pat)
    | Branching (_, branching) ->
      let space =
        branching.years |> List.map (search_space_of_year_range tz_offset_s)
      in
      Branching (space, branching)
    | Recur (_, recur) ->
      let space =
        match recur.year with
        | None -> default_search_space
        | Some year -> (
            match year with
            | Match l ->
              l
              |> Time.Year_ranges.Of_list.range_seq_of_list ~skip_sort:true
              |> Seq.map (search_space_of_year_range tz_offset_s)
              |> List.of_seq
            | Every_nth _ -> default_search_space )
      in
      Recur (space, recur)
    | Unary_op (_, op, t) -> (
        match op with
        | Not -> Unary_op (default_search_space, op, aux tz_offset_s t)
        | Change_tz_offset_s tz_offset_s ->
          let t = aux tz_offset_s t in
          Unary_op (get_search_space t, op, t)
        | _ ->
          let t = aux tz_offset_s t in
          Unary_op (get_search_space t, op, t) )
    | Inter_seq (_, s) ->
      let s = Seq.map (aux tz_offset_s) s in
      let space =
        s
        |> Seq.map get_search_space
        |> Seq.map List.to_seq
        |> Seq.map (Intervals.Normalize.normalize ~skip_sort:true)
        |> Intervals.Inter.inter_multi_seq ~skip_check:true
        |> List.of_seq
      in
      Inter_seq (space, s)
    | Interval_exc (_, start, end_exc) ->
      let space = [ (start, end_exc) ] in
      Interval_exc (space, start, end_exc)
    | Interval_inc (_, start, end_inc) ->
      let space = [ (start, Int64.succ @@ end_inc) ] in
      Interval_inc (space, start, end_inc)
    | Round_robin_pick_list (_, l) ->
      let space, l = aux_list tz_offset_s l in
      Round_robin_pick_list (space, l)
    | Union_seq (_, s) ->
      let space, s = aux_seq tz_offset_s s in
      Union_seq (space, s)
    | After (_, t1, t2) ->
      let space =
        [ t1; t2 ]
        |> List.map get_search_space
        |> List.map List.to_seq
        |> Intervals.Union.union_multi_list
        |> List.of_seq
      in
      After (space, t1, t2)
    | Between_inc (_, t1, t2) ->
      let space =
        [ t1; t2 ]
        |> List.map get_search_space
        |> List.map List.to_seq
        |> Intervals.Union.union_multi_list
        |> List.of_seq
      in
      Between_inc (space, t1, t2)
    | Between_exc (_, t1, t2) ->
      let space =
        [ t1; t2 ]
        |> List.map get_search_space
        |> List.map List.to_seq
        |> Intervals.Union.union_multi_list
        |> List.of_seq
      in
      Between_exc (space, t1, t2)
    | Unchunk c -> Unchunk c
  and aux_seq tz_offset_s s =
    let s = Seq.map (aux tz_offset_s) s in
    let space =
      Seq.map get_search_space s
      |> Seq.map List.to_seq
      |> Intervals.Union.union_multi_seq
      |> List.of_seq
    in
    (space, s)
  and aux_list tz_offset_s l =
    l
    |> List.to_seq
    |> aux_seq tz_offset_s
    |> fun (space, s) -> (space, List.of_seq s)
  in
  aux default_tz_offset_s time

let propagate_search_space_top_down (time : Time.t) : Time.t =
  let open Time in
  let restrict_search_space (parent : search_space) (cur : search_space) =
    Intervals.Inter.inter ~skip_check:true (List.to_seq parent)
      (List.to_seq cur)
    |> List.of_seq
  in
  let rec aux parent_search_space time =
    match time with
    | All -> All
    | Empty -> Empty
    | Timestamp_interval_seq (cur, s) ->
      Timestamp_interval_seq (restrict_search_space parent_search_space cur, s)
    | Pattern (cur, pat) ->
      Pattern (restrict_search_space parent_search_space cur, pat)
    | Branching (cur, branching) ->
      Branching (restrict_search_space parent_search_space cur, branching)
    | Recur (cur, recur) ->
      Recur (restrict_search_space parent_search_space cur, recur)
    | Unary_op (cur, op, t) ->
      let space = restrict_search_space parent_search_space cur in
      Unary_op (space, op, aux space t)
    | Interval_exc (cur, p1, p2) ->
      let space = restrict_search_space parent_search_space cur in
      Interval_exc (space, p1, p2)
    | Interval_inc (cur, p1, p2) ->
      let space = restrict_search_space parent_search_space cur in
      Interval_inc (space, p1, p2)
    | Round_robin_pick_list (cur, l) ->
      let space = restrict_search_space parent_search_space cur in
      Round_robin_pick_list (space, aux_list space l)
    | Inter_seq (cur, s) ->
      let space = restrict_search_space parent_search_space cur in
      Inter_seq (space, aux_seq space s)
    | Union_seq (cur, s) ->
      let space = restrict_search_space parent_search_space cur in
      Union_seq (space, aux_seq space s)
    | After (cur, t1, t2) ->
      let space = restrict_search_space parent_search_space cur in
      After (space, aux space t1, aux space t2)
    | Between_inc (cur, t1, t2) ->
      let space = restrict_search_space parent_search_space cur in
      Between_inc (space, aux space t1, aux space t2)
    | Between_exc (cur, t1, t2) ->
      let space = restrict_search_space parent_search_space cur in
      Between_exc (space, aux space t1, aux space t2)
    | Unchunk c -> Unchunk c
  and aux_list parent_search_space l = List.map (aux parent_search_space) l
  and aux_seq parent_search_space l = Seq.map (aux parent_search_space) l in
  aux default_search_space time

let optimize_search_space default_tz_offset_s t =
  t
  |> propagate_search_space_bottom_up default_tz_offset_s
  |> propagate_search_space_top_down

let positive_day_of_zero_or_negative_day ~day_count day =
  if day <= 0 then day + 1 + day_count else day

let intervals_of_branching tz_offset_s (space : Time.search_space)
    (b : Time.branching) : Time.Interval.t Seq.t =
  let open Time in
  let intervals_of_month_days year month month_days hmss =
    Seq.flat_map
      (fun day ->
         Seq.filter_map
           (fun hms_range ->
              match hms_range with
              | `Range_inc (start, end_inc) -> (
                  match
                    Date_time.make ~year ~month ~day ~hour:start.hour
                      ~minute:start.minute ~second:start.second ~tz_offset_s
                  with
                  | Error () -> None
                  | Ok dt1 -> (
                      match
                        Date_time.make ~year ~month ~day ~hour:end_inc.hour
                          ~minute:end_inc.minute ~second:end_inc.second
                          ~tz_offset_s
                      with
                      | Error () -> None
                      | Ok dt2 ->
                        Some
                          ( Date_time.to_timestamp dt1,
                            Int64.succ @@ Date_time.to_timestamp dt2 ) ) )
              | `Range_exc (start, end_exc) -> (
                  match
                    Date_time.make ~year ~month ~day ~hour:start.hour
                      ~minute:start.minute ~second:start.second ~tz_offset_s
                  with
                  | Error () -> None
                  | Ok dt1 -> (
                      match
                        Date_time.make ~year ~month ~day ~hour:end_exc.hour
                          ~minute:end_exc.minute ~second:end_exc.second
                          ~tz_offset_s
                      with
                      | Error () -> None
                      | Ok dt2 ->
                        Some
                          ( Date_time.to_timestamp dt1,
                            Date_time.to_timestamp dt2 ) ) ))
           hmss)
      month_days
  in
  let years_from_search_space =
    space
    |> List.to_seq
    |> Seq.flat_map (fun (x, y) ->
        let dt_x =
          Result.get_ok
          @@ Time.Date_time.of_timestamp
            ~tz_offset_s_of_date_time:tz_offset_s x
        in
        let dt_y =
          Result.get_ok
          @@ Time.Date_time.of_timestamp
            ~tz_offset_s_of_date_time:tz_offset_s y
        in
        fun () -> Seq.Cons (dt_x.year, Seq.return dt_y.year))
    |> Int_set.of_seq
  in
  let original_years =
    b.years |> List.to_seq |> Year_ranges.Flatten.flatten |> Int_set.of_seq
  in
  let years =
    Int_set.inter years_from_search_space original_years |> Int_set.to_seq
  in
  let months = b.months |> List.to_seq |> Month_ranges.Flatten.flatten in
  let hmss = b.hmss |> List.to_seq in
  match b.days with
  | Month_days days ->
    Seq.flat_map
      (fun year ->
         Seq.flat_map
           (fun month ->
              let day_count = day_count_of_month ~year ~month in
              let f_inc (x, y) =
                ( positive_day_of_zero_or_negative_day ~day_count x,
                  positive_day_of_zero_or_negative_day ~day_count y )
              in
              let f_exc (x, y) =
                ( positive_day_of_zero_or_negative_day ~day_count x,
                  positive_day_of_zero_or_negative_day ~day_count y )
              in
              let days =
                days
                |> List.to_seq
                |> Seq.map (Range.map ~f_inc ~f_exc)
                |> Month_day_ranges.normalize ~skip_filter_invalid:true
                |> Month_day_ranges.Flatten.flatten
              in
              intervals_of_month_days year month days hmss)
           months)
      years
  | Weekdays days ->
    let days = days |> Weekday_ranges.Flatten.flatten_list in
    Seq.flat_map
      (fun year ->
         let search_space = search_space_of_year tz_offset_s year in
         Seq.flat_map
           (fun month ->
              let month_days =
                {
                  Time.Pattern.years = Int_set.of_list [ year ];
                  months = Month_set.of_list [ month ];
                  month_days = Int_set.empty;
                  weekdays = Weekday_set.of_list days;
                  hours = Int_set.of_list [ 0 ];
                  minutes = Int_set.of_list [ 0 ];
                  seconds = Int_set.of_list [ 0 ];
                }
                |> Resolve_pattern.matching_date_times
                  (Search_param.make ~search_using_tz_offset_s:tz_offset_s
                     search_space)
                |> Seq.map (fun { Date_time.day; _ } -> day)
              in
              intervals_of_month_days year month month_days hmss)
           months)
      years

let resolve_arith_year_month_pairs ~year_start ~year_end_inc
    ~(start : Time.Date_time.t) n : (int * Time.month) Seq.t =
  let rec aux year year_end_inc month n =
    if year > year_end_inc then Seq.empty
    else
      let next_month = month + n in
      let next_year, next_month =
        if next_month < 12 then (year, next_month)
        else (succ year, next_month mod 12)
      in
      fun () ->
        Seq.Cons
          ( (year, Result.get_ok @@ Time.month_of_tm_int month),
            aux next_year year_end_inc next_month n )
  in
  let month_start = if year_start = start.year then start.month else `Jan in
  aux year_start year_end_inc (Time.tm_int_of_month month_start) n

(* let t_of_start_of_days_of_recur tz_offset_s (space : Time.search_space) (r : Time.recur) : Time.t
 *   =
 *   let open Time in
 *   let year_inc_range_from_space =
 *     match space with
 *     | [] -> None
 *     | l ->
 *       let start, _ = List.hd l in
 *       let _, end_exc = List.hd @@ List.rev l in
 *       let dt_x =
 *         Result.get_ok
 *         @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s start
 *       in
 *       let dt_y =
 *         Result.get_ok
 *         @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s
 *           (Int64.pred end_exc)
 *       in
 *       Some (dt_x.year, dt_y.year)
 *   in
 *   match year_inc_range_from_space with
 *   | None -> empty
 *   | Some (year_start_from_space, year_end_inc_from_space) -> (
 *       let year_ranges =
 *         let year_start =
 *           max r.start.year year_start_from_space |> min year_end_inc_from_space
 *         in
 *         match r.year with
 *         | None -> Seq.return (`Range_inc (year_start, year_end_inc_from_space))
 *         | Some year -> (
 *             match year with
 *             | Match l ->
 *               l
 *               |> List.filter (fun x ->
 *                   year_start <= x && x <= year_end_inc_from_space)
 *               |> Year_ranges.Of_list.range_seq_of_list ~skip_sort:true
 *             | Every_nth n ->
 *               OSeq.(year_start -- year_end_inc_from_space)
 *               |> OSeq.take_nth n
 *               |> Year_ranges.Of_seq.range_seq_of_seq ~skip_sort:true )
 *       in
 *       let year_inc_ranges =
 *         year_ranges
 *         |> Seq.map (fun year_range ->
 *             match year_range with
 *             | `Range_inc (x, y) -> (x, y)
 *             | `Range_exc (x, y) -> (x, pred y))
 *       in
 *       let one_day = Result.get_ok @@ Duration.make ~days:1 () in
 *       let years = Year_ranges.Flatten.flatten year_ranges in
 *       let hours = [ 0 ] in
 *       let minutes = [ 0 ] in
 *       let seconds = [ 0 ] in
 *       match (r.month, r.day) with
 *       | None, None -> pattern ~year_ranges:(List.of_seq year_ranges) ~hours ~minutes ~seconds ()
 *       | None, Some (Day (Match month_days)) ->
 *         pattern ~year_ranges:(List.of_seq year_ranges) ~month_days ~hours ~minutes ~seconds ()
 *       | None, Some (Day (Every_nth n)) ->
 *         pattern ~year_ranges:(List.of_seq year_ranges) ~hours ~minutes ~seconds ()
 *         |> chunk one_day
 *         |> take_nth n
 *       | None, Some (Weekday_every_nth (n, weekday)) ->
 *         pattern ~year_ranges:(List.of_seq year_ranges) ~weekdays:[ weekday ]
 *           ~hours ~minutes ~seconds
 *           ()
 *         |> chunk one_day
 *         |> take_nth n
 *       | None, Some (Weekday_nth (n, weekday)) ->
 *         pattern ~year_ranges:(List.of_seq year_ranges) ~weekdays:[ weekday ]
 *           ~hours ~minutes ~seconds
 *           ()
 *         |> chunk one_day
 *         |> nth n
 *       | Some (Match months), None ->
 *         pattern ~year_ranges:(List.of_seq year_ranges) ~months
 *           ~hours ~minutes ~seconds
 *           ()
 *       | Some (Match months), Some (Day (Match month_days)) ->
 *         pattern ~year_ranges:(List.of_seq year_ranges) ~months ~month_days
 *           ~hours ~minutes ~seconds
 *           ()
 *       | Some (Match months), Some (Day (Every_nth n)) ->
 *         let months = List.to_seq months in
 *         years
 *         |> Seq.flat_map (fun year ->
 *             Seq.map
 *               (fun month ->
 *                  pattern ~years:[ year ] ~months:[ month ]
 *                    ~hours ~minutes ~seconds
 *                    ()
 *                  |> chunk one_day
 *                  |> take_nth n)
 *               months)
 *         |> union_seq
 *       | Some (Match months), Some (Weekday_every_nth (n, weekday)) ->
 *         let months = List.to_seq months in
 *         years
 *         |> Seq.flat_map (fun year ->
 *             Seq.map
 *               (fun month ->
 *                  pattern ~years:[ year ] ~months:[ month ]
 *                    ~weekdays:[ weekday ]
 *                    ~hours ~minutes ~seconds
 *                    ()
 *                  |> chunk one_day
 *                  |> take_nth n)
 *               months)
 *         |> union_seq
 *       | Some (Match months), Some (Weekday_nth (n, weekday)) ->
 *         let months = List.to_seq months in
 *         years
 *         |> Seq.flat_map (fun year ->
 *             Seq.map
 *               (fun month ->
 *                  pattern ~years:[ year ] ~months:[ month ]
 *                    ~weekdays:[ weekday ]
 *                    ~hours ~minutes ~seconds
 *                    ()
 *                  |> chunk one_day
 *                  |> nth n)
 *               months)
 *         |> union_seq
 *       | Some (Every_nth n), None ->
 *         year_inc_ranges
 *         |> Seq.flat_map (fun (year_start, year_end_inc) ->
 *             resolve_arith_year_month_pairs ~year_start ~year_end_inc
 *               ~start:r.start n
 *             |> Seq.map (fun (year, month) ->
 *                 pattern ~years:[ year ] ~months:[ month ]
 *                   ~hours ~minutes ~seconds
 *                   ()))
 *         |> union_seq
 *       | Some (Every_nth n), Some (Day (Match month_days)) ->
 *         year_inc_ranges
 *         |> Seq.flat_map (fun (year_start, year_end_inc) ->
 *             resolve_arith_year_month_pairs ~year_start ~year_end_inc
 *               ~start:r.start n
 *             |> Seq.map (fun (year, month) ->
 *                 pattern ~years:[ year ] ~months:[ month ] ~month_days
 *                   ~hours ~minutes ~seconds
 *                   ()))
 *         |> union_seq
 *       | Some (Every_nth month_n), Some (Day (Every_nth day_n)) ->
 *         year_inc_ranges
 *         |> Seq.flat_map (fun (year_start, year_end_inc) ->
 *             resolve_arith_year_month_pairs ~year_start ~year_end_inc
 *               ~start:r.start month_n
 *             |> Seq.map (fun (year, month) ->
 *                 pattern ~years:[ year ] ~months:[ month ]
 *                   ~hours ~minutes ~seconds
 *                   ()
 *                 |> chunk one_day
 *                 |> take_nth day_n))
 *         |> union_seq
 *       | Some (Every_nth month_n), Some (Weekday_every_nth (n, weekday)) ->
 *         year_inc_ranges
 *         |> Seq.flat_map (fun (year_start, year_end_inc) ->
 *             resolve_arith_year_month_pairs ~year_start ~year_end_inc
 *               ~start:r.start month_n
 *             |> Seq.map (fun (year, month) ->
 *                 pattern ~years:[ year ] ~months:[ month ]
 *                   ~weekdays:[ weekday ]
 *                   ~hours ~minutes ~seconds
 *                   ()
 *                 |> chunk one_day
 *                 |> take_nth n))
 *         |> union_seq
 *       | Some (Every_nth month_n), Some (Weekday_nth (n, weekday)) ->
 *         year_inc_ranges
 *         |> Seq.flat_map (fun (year_start, year_end_inc) ->
 *             resolve_arith_year_month_pairs ~year_start ~year_end_inc
 *               ~start:r.start month_n
 *             |> Seq.map (fun (year, month) ->
 *                 pattern ~years:[ year ] ~months:[ month ]
 *                   ~weekdays:[ weekday ]
 *                   ~hours ~minutes ~seconds
 *                   ()
 *                 |> chunk one_day
 *                 |> nth n))
 *         |> union_seq ) *)

type inc_or_exc =
  | Inc
  | Exc

let do_skip_n_points (n : int64) (s : Time.Interval.t Seq.t) :
  Time.Interval.t Seq.t =
  let rec aux n s =
    if n = 0L then s
    else
      match s () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((x, y), rest) ->
        let size = Int64.sub y x in
        if size >= n then fun () -> Seq.Cons ((Int64.add n x, y), rest)
        else aux (Int64.sub n size) rest
  in
  aux n s

let do_take_n_points (n : int64) (s : Time.Interval.t Seq.t) :
  Time.Interval.t Seq.t =
  let rec aux n s =
    if n = 0L then Seq.empty
    else
      match s () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((x, y), rest) ->
        let size = Int64.sub y x in
        if size >= n then Seq.return (x, Int64.add n x)
        else fun () -> Seq.Cons ((x, y), aux (Int64.sub n size) rest)
  in
  aux n s

let find_after ((_start, end_exc) : Time.Interval.t)
    (s2 : Time.Interval.t Seq.t) =
  match OSeq.drop_while (fun (start', _) -> start' < end_exc) s2 () with
  | Seq.Nil -> None
  | Seq.Cons (x, _) -> Some x

let do_chunk_by_year tz_offset_s (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 = Result.get_ok @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s t1 in
      let dt2 = Result.get_ok @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s t2 in
      if dt1.year = dt2.year && dt1.month = dt2.month then
        fun () -> Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time.set_to_last_day_hour_min_sec dt1
          |> Date_time.to_timestamp
          |> Int64.succ in
        OSeq.cons (t1, t')
          (aux (OSeq.cons (t', t2) rest))
  in
  aux s

let do_chunk_by_month tz_offset_s (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 = Result.get_ok @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s t1 in
      let dt2 = Result.get_ok @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s t2 in
      if dt1.year = dt2.year && dt1.month = dt2.month then
        fun () -> Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time.set_to_last_day_hour_min_sec dt1
          |> Date_time.to_timestamp
          |> Int64.succ in
        OSeq.cons (t1, t')
          (aux (OSeq.cons (t', t2) rest))
  in
  aux s

let resolve ?(search_using_tz_offset_s = 0) (time : Time.t) :
  (Time.Interval.t Seq.t, string) result =
  let rec aux search_using_tz_offset_s time =
    let open Time in
    match time with
    | Empty -> Seq.empty
    | All ->
      Seq.return Date_time.(to_timestamp min, Int64.succ @@ to_timestamp max)
    | Timestamp_interval_seq (_, s) -> s
    | Pattern (space, pat) ->
      let params =
        List.map (Search_param.make ~search_using_tz_offset_s) space
      in
      Intervals.Union.union_multi_list ~skip_check:true
        (List.map
           (fun param -> Resolve_pattern.matching_intervals param pat)
           params)
    | Branching (space, branching) ->
      Intervals.Inter.inter ~skip_check:true (List.to_seq space)
        (intervals_of_branching search_using_tz_offset_s space branching)
    | Recur (space, recur) ->
      failwith "Unimplemented"
      (* let one_day = Result.get_ok @@ Duration.make ~days:1 () in
       * let one_day_in_seconds = Duration.to_seconds one_day in
       * t_of_start_of_days_of_recur search_using_tz_offset_s space recur
       * |> aux search_using_tz_offset_s *)
      (* |> (
       *     match recur.hms with
       *     | None -> Seq.map (fun (x, _) -> (x, Int64.add x one_day_in_seconds))
       *     | Some hms ->
       *       let dt = Result.get_ok @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:search_using_tz_offset_s x in
       *       match hms with
       *       | Every_nth { hour; minute; second } ->
       *         failwith "Unimplemented"
       *       | Hmss ranges ->
       *         let f (x, y) =
       *           second_of_day_of_hms x, second_of_day_of_hms y
       *         in
       *         let second_of_day_ranges_exc =
       *           ranges
       *           |> List.map (
       *             Range.map ~f_inc:f ~f_exc:f
       *             )
       *           |> List.map (fun range ->
       *               match range with
       *               | `Range_exc (x, y) -> (x, y)
       *               | `Range_inc (x, y) -> (x, succ y)
       *             )
       *           |> List.map (fun (x, y) ->
       *               Int64.of_int x, Int64.of_int y
       *             )
       *         in
       *         Seq.flat_map (fun (x, _) ->
       *             second_of_day_ranges_exc
       *             |> List.map (fun (second_of_day_start, second_of_day_end_exc) ->
       *                 (Int64.add x second_of_day_start,
       *                  Int64.add x second_of_day_end_exc
       *                 )
       *               )
       *             |> List.to_seq
       *           )
       *   ) *)
      (* |> Intervals.Inter.inter ~skip_check:true (List.to_seq space) *)
    | Unary_op (space, op, t) -> (
        let search_using_tz_offset_s =
          match op with
          | Change_tz_offset_s x -> x
          | _ -> search_using_tz_offset_s
        in
        let s = aux search_using_tz_offset_s t in
        match op with
        | Not ->
          Intervals.relative_complement ~skip_check:true ~not_mem_of:s
            (List.to_seq space)
        | Every -> s
        | Skip_n_points n ->
          do_skip_n_points (Int64.of_int n) s
          |> Intervals.Normalize.normalize ~skip_filter_empty:true
            ~skip_sort:true ~skip_filter_invalid:true
        (* | Skip_n_intervals n -> OSeq.drop n s *)
        | Next_n_points n -> do_take_n_points (Int64.of_int n) s
        (* | Next_n_intervals n -> OSeq.take n s *)
        (* | Every_nth n -> OSeq.take_nth n s *)
        (* | Nth n -> s |> OSeq.drop n |> OSeq.take 1
         * | Chunk { chunk_size; drop_partial } ->
         *   Intervals.chunk ~skip_check:true ~drop_partial ~chunk_size s
         * | Chunk_by_year ->
         *   do_chunk_by_year search_using_tz_offset_s s
         * | Chunk_by_month ->
         *   do_chunk_by_month search_using_tz_offset_s s *)
        | Shift n ->
          Seq.map
            (fun (start, end_exc) -> (Int64.add start n, Int64.add end_exc n))
            s
        | Lengthen n ->
          s
          |> Seq.map (fun (start, end_exc) -> (start, Int64.add end_exc n))
          |> Intervals.Normalize.normalize ~skip_filter_empty:true
            ~skip_sort:true ~skip_filter_invalid:true
        | Change_tz_offset_s _ -> s )
    | Interval_inc (_, a, b) -> Seq.return (a, Int64.succ b)
    | Interval_exc (_, a, b) -> Seq.return (a, b)
    | Round_robin_pick_list (_, l) ->
      List.map (aux search_using_tz_offset_s) l
      |> Time.Intervals.Round_robin
         .merge_multi_list_round_robin_non_decreasing ~skip_check:true
    | Inter_seq (_, s) ->
      Seq.map (aux search_using_tz_offset_s) s
      |> Time.Intervals.Inter.inter_multi_seq ~skip_check:true
    | Union_seq (_, s) ->
      Seq.map (aux search_using_tz_offset_s) s
      |> Time.Intervals.Union.union_multi_seq ~skip_check:true
    | After (_, t1, t2) ->
      let s1 = aux search_using_tz_offset_s t1 in
      let s2 = aux search_using_tz_offset_s t2 in
      s1 |> Seq.filter_map (fun x -> find_after x s2)
    | Between_inc (_, t1, t2) ->
      let s1 = aux search_using_tz_offset_s t1 in
      let s2 = aux search_using_tz_offset_s t2 in
      s1
      |> Seq.filter_map (fun (start, end_exc) ->
          find_after (start, end_exc) s2
          |> Option.map (fun (_, end_exc') -> (start, end_exc')))
    | Between_exc (_, t1, t2) ->
      let s1 = aux search_using_tz_offset_s t1 in
      let s2 = aux search_using_tz_offset_s t2 in
      s1
      |> Seq.filter_map (fun (start, end_exc) ->
          find_after (start, end_exc) s2
          |> Option.map (fun (start', _) -> (start, start')))
    | Unchunk _ ->
      failwith "Unimplemented"
  in
  try
    time
    |> optimize_search_space search_using_tz_offset_s
    |> aux search_using_tz_offset_s
    |> Time.Intervals.Normalize.normalize ~skip_filter_invalid:true
      ~skip_sort:true
    |> Result.ok
  with
  | Time.Interval_is_invalid -> Error "Invalid interval"
  | Time.Intervals_are_not_sorted -> Error "Intervals are not sorted"
