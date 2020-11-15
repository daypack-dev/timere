module Search_param = struct
  type t = {
    search_using_tz_offset_s : Time.tz_offset_s option;
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
        (t : Time.Pattern.pattern) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      match t.seconds with
      | [] ->
        Seq.map
          (fun second -> { cur_branch with second })
          OSeq.(cur_branch_search_start.second --^ 60)
      | pat_sec_list ->
        pat_sec_list
        |> List.to_seq
        |> Seq.filter (fun second ->
            cur_branch_search_start.second <= second && second < 60)
        |> Seq.map (fun second -> { cur_branch with second })

    let matching_second_ranges (t : Time.Pattern.pattern)
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
      match t.seconds with
      | [] ->
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_sec cur_branch ))
      | l ->
        List.sort_uniq compare l
        |> Time.Second_ranges.Of_list.range_seq_of_list
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
        (t : Time.Pattern.pattern) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      match t.minutes with
      | [] ->
        Seq.map
          (fun minute -> { cur_branch with minute })
          OSeq.(cur_branch_search_start.minute --^ 60)
      | pat_min_list ->
        pat_min_list
        |> List.to_seq
        |> Seq.filter (fun minute ->
            cur_branch_search_start.minute <= minute && minute < 60)
        |> Seq.map (fun minute -> { cur_branch_search_start with minute })

    let matching_minute_ranges ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.pattern) (cur_branch : Time.Date_time.t) :
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
      match t.minutes with
      | [] ->
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_min_sec cur_branch_search_start ))
      | l ->
        List.filter (fun min -> cur_branch_search_start.minute <= min) l
        |> List.sort_uniq compare
        |> Time.Minute_ranges.Of_list.range_seq_of_list
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

    let matching_hours (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      match t.hours with
      | [] ->
        Seq.map
          (fun hour -> { cur_branch with hour })
          OSeq.(cur_branch_search_start.hour --^ 24)
      | pat_hour_list ->
        pat_hour_list
        |> List.to_seq
        |> Seq.filter (fun hour ->
            cur_branch_search_start.hour <= hour && hour < 24)
        |> Seq.map (fun hour -> { cur_branch with hour })

    let matching_hour_ranges ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.pattern) (cur_branch : Time.Date_time.t) :
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
      match t.hours with
      | [] ->
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_hour_min_sec cur_branch ))
      | l ->
        List.filter
          (fun hour -> cur_branch_search_start.hour <= hour && hour < 24)
          l
        |> List.sort_uniq compare
        |> Time.Hour_ranges.Of_list.range_seq_of_list
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
        ~(cur_branch_search_start : Time.Date_time.t) (t : Time.Pattern.pattern)
      : int Seq.t =
      let day_count =
        Time.day_count_of_month ~year:cur_branch_search_start.year
          ~month:cur_branch_search_start.month
      in
      match t.weekdays with
      | [] -> OSeq.(cur_branch_search_start.day -- day_count)
      | l ->
        OSeq.(cur_branch_search_start.day -- day_count)
        |> Seq.filter (fun mday ->
            match
              Time.weekday_of_month_day ~year:cur_branch_search_start.year
                ~month:cur_branch_search_start.month ~mday
            with
            | Ok wday -> List.mem wday l
            | Error () -> false)

    let direct_matching_int_month_days
        ~(cur_branch_search_start : Time.Date_time.t) (t : Time.Pattern.pattern)
      : int Seq.t =
      let day_count =
        Time.day_count_of_month ~year:cur_branch_search_start.year
          ~month:cur_branch_search_start.month
      in
      match t.month_days with
      | [] -> OSeq.(cur_branch_search_start.day -- day_count)
      | l ->
        List.filter
          (fun mday ->
             cur_branch_search_start.day <= mday && mday <= day_count)
          l
        |> List.sort_uniq compare
        |> List.to_seq

    let matching_int_month_days ~(cur_branch_search_start : Time.Date_time.t)
        (t : Time.Pattern.pattern) : int Seq.t =
      let matching_month_days =
        direct_matching_int_month_days t ~cur_branch_search_start
        |> List.of_seq
        |> List.sort_uniq compare
      in
      let month_days_of_matching_weekdays =
        int_month_days_of_matching_weekdays t ~cur_branch_search_start
        |> List.of_seq
        |> List.sort_uniq compare
      in
      OSeq.(1 -- 31)
      |> Seq.filter (fun mday ->
          List.mem mday matching_month_days
          && List.mem mday month_days_of_matching_weekdays)

    let matching_days ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.pattern) (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      matching_int_month_days t ~cur_branch_search_start
      |> Seq.map (fun day -> { cur_branch_search_start with day })

    let matching_day_ranges ~(overall_search_start : Time.Date_time.t)
        (t : Time.Pattern.pattern) (cur_branch : Time.Date_time.t) :
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
      match (t.month_days, t.weekdays) with
      | [], [] ->
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_day_hour_min_sec cur_branch ))
      | [], _weekdays ->
        int_month_days_of_matching_weekdays t ~cur_branch_search_start
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
      | _month_days, [] ->
        direct_matching_int_month_days t ~cur_branch_search_start
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
      | _, _ ->
        matching_int_month_days t ~cur_branch_search_start
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
  end

  module Matching_months = struct
    let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if cur_branch.year = overall_search_start.year then overall_search_start
      else Time.Date_time.set_to_first_month_day_hour_min_sec cur_branch

    let matching_months (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let start_month_int =
        Time.human_int_of_month cur_branch_search_start.month
      in
      match t.months with
      | [] ->
        OSeq.(start_month_int -- 12)
        |> Seq.map (fun month ->
            Time.month_of_human_int month |> Result.get_ok)
        |> Seq.map (fun month -> { cur_branch_search_start with month })
      | pat_mon_list ->
        pat_mon_list
        |> List.to_seq
        |> Seq.map Time.human_int_of_month
        |> Seq.filter (fun month -> start_month_int <= month && month <= 12)
        |> Seq.map (fun month ->
            Time.month_of_human_int month |> Result.get_ok)
        |> Seq.map (fun month -> { cur_branch_search_start with month })

    let matching_month_ranges (t : Time.Pattern.pattern)
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
      match t.months with
      | [] ->
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_month_day_hour_min_sec
                 cur_branch_search_start ))
      | l ->
        l
        |> List.sort_uniq Time.compare_month
        |> List.to_seq
        |> Seq.map Time.human_int_of_month
        |> Seq.filter (fun month -> start_month_int <= month && month <= 12)
        |> Seq.map (fun month ->
            Time.month_of_human_int month |> Result.get_ok)
        |> Time.Month_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:(range_map_inc ~cur_branch_search_start)
             ~f_exc:(range_map_exc ~cur_branch_search_start))
  end

  module Matching_years = struct
    let matching_years ~(overall_search_start : Time.Date_time.t) ~end_year_inc
        (t : Time.Pattern.pattern) : Time.Date_time.t Seq.t =
      match t.years with
      | [] ->
        OSeq.(overall_search_start.year -- end_year_inc)
        |> Seq.map (fun year -> { overall_search_start with year })
      | pat_year_list ->
        pat_year_list
        |> List.to_seq
        |> Seq.filter (fun year ->
            overall_search_start.year <= year && year <= end_year_inc)
        |> Seq.map (fun year -> { overall_search_start with year })

    let matching_year_ranges ~(overall_search_start : Time.Date_time.t)
        ~end_year_inc (t : Time.Pattern.pattern) :
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
      match t.years with
      | [] ->
        Seq.return
          (`Range_inc
             ( overall_search_start,
               Time.Date_time.set_to_last_month_day_hour_min_sec
                 { overall_search_start with year = end_year_inc } ))
      | l ->
        List.sort_uniq compare l
        |> List.to_seq
        |> Seq.filter (fun year ->
            overall_search_start.year <= year && year <= end_year_inc)
        |> Time.Year_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:(range_map_inc ~overall_search_start)
             ~f_exc:(range_map_exc ~overall_search_start))
  end

  module Matching_timestamps = struct
    let matching_timestamps
        ~(search_using_tz_offset_s : Time.tz_offset_s option)
        (t : Time.Pattern.pattern) (start : Time.Date_time.t) :
      Time.Date_time_set.t =
      match Time.Date_time.to_timestamp start with
      | Error () -> Time.Date_time_set.empty
      | Ok start ->
        t.timestamps
        |> List.sort_uniq compare
        |> List.to_seq
        |> OSeq.filter (fun x -> x >= start)
        |> Seq.filter_map (fun x ->
            match
              Time.Date_time.of_timestamp
                ~tz_offset_s_of_date_time:search_using_tz_offset_s x
            with
            | Ok x -> Some x
            | Error () -> None)
        |> Time.Date_time_set.of_seq
  end

  let filter_using_matching_timestamps ~search_using_tz_offset_s
      (t : Time.Pattern.pattern) ~overall_search_start
      (s : Time.Date_time.t Seq.t) : Time.Date_time.t Seq.t =
    let matching_timestamps =
      Matching_timestamps.matching_timestamps ~search_using_tz_offset_s t
        overall_search_start
    in
    if Time.Date_time_set.is_empty matching_timestamps then s
    else Seq.filter (fun x -> Time.Date_time_set.mem x matching_timestamps) s

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

  let matching_date_times (search_param : Search_param.t)
      (pat : Time.Pattern.pattern) : Time.Date_time.t Seq.t =
    let search_using_tz_offset_s = search_param.search_using_tz_offset_s in
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
    |> filter_using_matching_timestamps ~search_using_tz_offset_s pat
      ~overall_search_start

  let matching_timestamps (search_param : Search_param.t)
      (t : Time.Pattern.pattern) : int64 Seq.t =
    matching_date_times search_param t
    |> Seq.filter_map (fun x ->
        match Time.Date_time.to_timestamp x with
        | Ok x -> Some x
        | Error () -> None)

  let matching_date_time_ranges (search_param : Search_param.t)
      (t : Time.Pattern.pattern) : Time.Date_time.t Time.Range.range Seq.t =
    let overall_search_start = search_param.start in
    let search_using_tz_offset_s = search_param.search_using_tz_offset_s in
    let end_year_inc = search_param.end_inc.year in
    match
      ( t.years,
        t.months,
        t.month_days,
        t.weekdays,
        t.hours,
        t.minutes,
        t.seconds,
        t.timestamps )
    with
    | _years, [], [], [], [], [], [], [] ->
      Matching_years.matching_year_ranges ~overall_search_start ~end_year_inc
        t
    | _years, _months, [], [], [], [], [], [] ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_month_ranges t ~overall_search_start)
    | _years, _months, _month_days, _weekdays, [], [], [], [] ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map
        (Matching_days.matching_day_ranges t ~overall_search_start)
    | _years, _months, _month_days, _weekdays, _hours, [], [], [] ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map (Matching_days.matching_days t ~overall_search_start)
      |> Seq.flat_map
        (Matching_hours.matching_hour_ranges t ~overall_search_start)
    | _years, _months, _month_days, _weekdays, _hours, _minutes, [], [] ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map (Matching_days.matching_days t ~overall_search_start)
      |> Seq.flat_map (Matching_hours.matching_hours t ~overall_search_start)
      |> Seq.flat_map
        (Matching_minutes.matching_minute_ranges t ~overall_search_start)
    | _years, _months, _month_days, _weekdays, _hours, _minutes, _seconds, [] ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map (Matching_days.matching_days t ~overall_search_start)
      |> Seq.flat_map (Matching_hours.matching_hours t ~overall_search_start)
      |> Seq.flat_map
        (Matching_minutes.matching_minutes t ~overall_search_start)
      |> Seq.flat_map
        (Matching_seconds.matching_second_ranges t ~overall_search_start)
    | [], [], [], [], [], [], [], timestamps ->
      timestamps
      |> List.to_seq
      |> date_time_range_seq_of_timestamps ~search_using_tz_offset_s
    | ( _years,
        _months,
        _month_days,
        _weekdays,
        _hours,
        _minutes,
        _seconds,
        _timestamps ) ->
      Matching_years.matching_years ~overall_search_start ~end_year_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start)
      |> Seq.flat_map (Matching_days.matching_days t ~overall_search_start)
      |> Seq.flat_map (Matching_hours.matching_hours t ~overall_search_start)
      |> Seq.flat_map
        (Matching_minutes.matching_minutes t ~overall_search_start)
      |> Seq.flat_map
        (Matching_seconds.matching_seconds t ~overall_search_start)
      |> filter_using_matching_timestamps ~search_using_tz_offset_s t
        ~overall_search_start
      |> Seq.map (fun x -> `Range_inc (x, x))

  let matching_intervals (search_param : Search_param.t)
      (t : Time.Pattern.pattern) : (int64 * int64) Seq.t =
    let f (x, y) =
      (Time.Date_time.to_timestamp x, Time.Date_time.to_timestamp y)
    in
    matching_date_time_ranges search_param t
    |> Seq.map (Time.Range.map ~f_inc:f ~f_exc:f)
    |> Seq.filter_map Time.Range_utils.result_range_get
    |> Seq.map (fun r ->
        match r with
        | `Range_inc (x, y) -> (x, Int64.succ y)
        | `Range_exc (x, y) -> (x, y))

  let matching_intervals_round_robin_non_decreasing
      (search_param : Search_param.t) (l : Time.Pattern.pattern list) :
    ((int64 * int64) list Seq.t, error) result =
    let l = List.map (matching_intervals search_param) l in
    l
    |> Time.Intervals.Round_robin.collect_round_robin_non_decreasing
      ~skip_check:true
    |> OSeq.take_while (List.for_all Option.is_some)
    |> Seq.map (List.map Option.get)
    |> Result.ok

  let matching_intervals_round_robin_non_decreasing_flat
      (search_param : Search_param.t) (l : Time.Pattern.pattern list) :
    ((int64 * int64) Seq.t, error) result =
    matching_intervals_round_robin_non_decreasing search_param l
    |> Result.map (Seq.flat_map List.to_seq)

  let next_match_date_time (search_param : Search_param.t)
      (t : Time.Pattern.pattern) : Time.Date_time.t option =
    let s = matching_date_times search_param t in
    match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x

  let next_match_timestamp (search_param : Search_param.t)
      (t : Time.Pattern.pattern) : int64 option =
    match next_match_date_time search_param t with
    | None -> None
    | Some x -> (
        match Time.Date_time.to_timestamp x with
        | Error () -> None
        | Ok x -> Some x )

  let next_match_interval (search_param : Search_param.t)
      (t : Time.Pattern.pattern) : (int64 * int64) option =
    let s = matching_intervals search_param t in
    match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x
end

let get_search_space (time : Time.t) : Time.Interval.t list =
  let open Time in
  match time with
  | Timestamp_interval_seq (s, _) -> s
  | Pattern (s, _) -> s
  | Branching (s, _) -> s
  | Unary_op (s, _, _) -> s
  | Binary_op (s, _, _, _) -> s
  | Round_robin_pick_list (s, _) -> s
  | Merge_list (s, _) -> s

let set_search_space space (time : Time.t) : Time.t =
  let open Time in
  match time with
  | Timestamp_interval_seq (_, x) -> Timestamp_interval_seq (space, x)
  | Pattern (_, x) -> Pattern (space, x)
  | Branching (_, x) -> Branching (space, x)
  | Unary_op (_, op, x) -> Unary_op (space, op, x)
  | Binary_op (_, op, x, y) -> Binary_op (space, op, x, y)
  | Round_robin_pick_list (_, x) -> Round_robin_pick_list (space, x)
  | Merge_list (_, x) -> Merge_list (space, x)

let empty_search_space = []

let propagate_search_space_bottom_up (time : Time.t) : Time.t =
  let open Time in
  let rec aux time =
    match time with
    | Timestamp_interval_seq (_, s) -> (
        match s () with
        | Seq.Nil -> time
        | Seq.Cons ((start, _), _) ->
          Timestamp_interval_seq ([ (start, default_search_space_end_exc) ], s)
      )
    | Pattern (_, pat) -> (
        let space_for_years =
          pat.years
          |> List.to_seq
          |> Seq.map (fun year ->
              ( Date_time.set_to_first_month_day_hour_min_sec
                  { Date_time.min with year },
                Date_time.set_to_last_month_day_hour_min_sec
                  { Date_time.min with year } ))
          |> Seq.map (fun (dt1, dt2) ->
              ( Result.get_ok @@ Date_time.to_timestamp dt1,
                Int64.succ @@ Result.get_ok @@ Date_time.to_timestamp dt2 ))
        in
        let space_for_timestamps =
          pat.timestamps |> List.to_seq |> Seq.map (fun x -> (x, Int64.succ x))
        in
        match (pat.years, pat.timestamps) with
        | [], [] -> time
        | _years, [] -> Pattern (List.of_seq space_for_years, pat)
        | [], _timestamps -> Pattern (List.of_seq space_for_timestamps, pat)
        | _years, _timestamps ->
          let space =
            Intervals.inter ~skip_check:true space_for_timestamps
              space_for_years
            |> List.of_seq
          in
          Pattern (space, pat) )
    | Branching _ -> failwith "Unimplemented"
    | Unary_op (space, op, t) -> (
        match op with
        | Not -> Unary_op (default_search_space, op, aux t)
        | _ -> Unary_op (space, op, aux t) )
    | Binary_op (_, op, t1, t2) -> (
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t1_search_space = get_search_space t1 in
        let t2_search_space = get_search_space t2 in
        match op with
        | Inter ->
          let space =
            Intervals.inter
              (List.to_seq t1_search_space)
              (List.to_seq t2_search_space)
            |> List.of_seq
          in
          Binary_op (space, Inter, t1, t2)
        | Interval_inc | Interval_exc | Intervals_inc | Intervals_exc -> (
            match t1_search_space with
            | [] -> Binary_op ([], op, t1, t2)
            | (t1_start, _) :: _ ->
              let t2_search_space =
                Intervals.inter
                  (Seq.return (t1_start, default_search_space_end_exc))
                  (List.to_seq t2_search_space)
              in
              let t2 = set_search_space (List.of_seq t2_search_space) t2 in
              let space =
                Intervals.Union.union
                  (List.to_seq t1_search_space)
                  t2_search_space
                |> List.of_seq
              in
              Binary_op (space, op, t1, t2) )
        | _ ->
          let space =
            Intervals.Union.union
              (List.to_seq t1_search_space)
              (List.to_seq t2_search_space)
            |> List.of_seq
          in
          Binary_op (space, op, t1, t2) )
    | Round_robin_pick_list (_, l) ->
      let space, l = aux_list l in
      Round_robin_pick_list (space, l)
    | Merge_list (_, l) ->
      let space, l = aux_list l in
      Merge_list (space, l)
  and aux_list l =
    let l = List.map aux l in
    let space =
      List.map get_search_space l
      |> List.map List.to_seq
      |> Intervals.Merge.merge_multi_list
      |> List.of_seq
    in
    (space, l)
  in
  aux time

let propagate_search_space_top_down (time : Time.t) : Time.t =
  let open Time in
  let restrict_search_space (parent : search_space) (cur : search_space) =
    Intervals.inter ~skip_check:true (List.to_seq parent) (List.to_seq cur)
    |> List.of_seq
  in
  let rec aux parent_search_space time =
    match time with
    | Timestamp_interval_seq (cur, s) ->
      Timestamp_interval_seq (restrict_search_space parent_search_space cur, s)
    | Pattern (cur, pat) ->
      Pattern (restrict_search_space parent_search_space cur, pat)
    | Branching _ -> failwith "Unimplemented"
    | Unary_op (cur, op, t) ->
      let space = restrict_search_space parent_search_space cur in
      Unary_op (space, op, aux space t)
    | Binary_op (cur, op, t1, t2) ->
      let space = restrict_search_space parent_search_space cur in
      Binary_op (space, op, aux space t1, aux space t2)
    | Round_robin_pick_list (cur, l) ->
      let space = restrict_search_space parent_search_space cur in
      Round_robin_pick_list (space, aux_list space l)
    | Merge_list (cur, l) ->
      let space = restrict_search_space parent_search_space cur in
      Merge_list (space, aux_list space l)
  and aux_list parent_search_space l = List.map (aux parent_search_space) l in
  aux default_search_space time

let optimize_search_space t =
  t |> propagate_search_space_bottom_up |> propagate_search_space_top_down

let resolve ?search_using_tz_offset_s (time : Time.t) :
  (Time.Interval.t Seq.t, string) result =
  let rec aux time =
    let open Time in
    match time with
    | Timestamp_interval_seq (_, s) -> Ok s
    | Pattern (space, pat) ->
      let params =
        List.map (Search_param.make ~search_using_tz_offset_s) space
      in
      Ok
        (Intervals.Union.union_multi_list ~skip_check:true
           (List.map
              (fun param -> Resolve_pattern.matching_intervals param pat)
              params))
    | Branching (_, _branching) -> failwith "Unimplemented"
    | Unary_op (space, op, t) ->
      aux t
      |> Result.map (fun s ->
          match op with
          | Not ->
            Intervals.relative_complement ~skip_check:true ~not_mem_of:s
              (List.to_seq space)
          | Every -> s
          | Chunk { chunk_size; drop_partial } ->
            Intervals.chunk ~skip_check:true ~drop_partial ~chunk_size s
          | _ -> failwith "Unimplemented")
    | Binary_op (_, op, t1, t2) -> (
        match aux t1 with
        | Error msg -> Error msg
        | Ok s1 -> (
            match aux t2 with
            | Error msg -> Error msg
            | Ok s2 ->
              Ok
                ( match op with
                  | Union -> Intervals.Union.union ~skip_check:true s1 s2
                  | Inter -> Intervals.inter ~skip_check:true s1 s2
                  | _ -> failwith "Unimplemented" ) ) )
    | Round_robin_pick_list (_, l) ->
      Misc_utils.get_ok_error_list (List.map aux l)
      |> Result.map
        (Time.Intervals.Round_robin
         .merge_multi_list_round_robin_non_decreasing ~skip_check:true)
    | Merge_list (_, l) ->
      Misc_utils.get_ok_error_list (List.map aux l)
      |> Result.map (Time.Intervals.Merge.merge_multi_list ~skip_check:true)
  in
  aux (optimize_search_space time)
