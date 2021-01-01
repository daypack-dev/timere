module Resolve_pattern = struct
  module Search_param = struct
    type t = {
      search_using_tz_offset_s : int;
      start : Time.Date_time.t;
      end_inc : Time.Date_time.t;
    }

    let make ~search_using_tz_offset_s ~search_using_tz
        ((start, end_exc) : Time.Interval.t) : t =
      {
        search_using_tz_offset_s;
        start =
          Result.get_ok
          @@ Time.Date_time.of_timestamp ~tz_of_date_time:search_using_tz start;
        end_inc =
          Result.get_ok
          @@ Time.Date_time.of_timestamp ~tz_of_date_time:search_using_tz
            (Int64.pred end_exc);
      }
  end

  let failwith_unexpected_case (_ : 'a) = failwith "Unexpected case"

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

    let get_cur_branch_search_end_inc
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_end_inc.year
        && cur_branch.month = overall_search_end_inc.month
        && cur_branch.day = overall_search_end_inc.day
        && cur_branch.hour = overall_search_end_inc.hour
        && cur_branch.minute = overall_search_end_inc.minute
      then overall_search_end_inc
      else Time.Date_time.set_to_last_sec cur_branch

    let filter_seconds ~(cur_branch_search_start : Time.Date_time.t)
        ~(cur_branch_search_end_inc : Time.Date_time.t) s =
      Seq.filter
        (fun sec ->
           cur_branch_search_start.second <= sec
           && sec <= cur_branch_search_end_inc.second)
        s

    let matching_seconds ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      if Int_set.is_empty t.seconds then
        Seq.map
          (fun second -> { cur_branch with second })
          OSeq.(
            cur_branch_search_start.second -- cur_branch_search_end_inc.second)
      else
        t.seconds
        |> Int_set.to_seq
        |> filter_seconds ~cur_branch_search_start ~cur_branch_search_end_inc
        |> Seq.map (fun second -> { cur_branch with second })

    let matching_second_ranges (t : Time.Pattern.t)
        ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t)
          ~(cur_branch_search_end_inc : Time.Date_time.t) (x, y) =
        let range_map_start =
          if x = cur_branch_search_start.second then cur_branch_search_start
          else { cur_branch_search_start with second = x }
        in
        let range_map_end_inc =
          if y = cur_branch_search_end_inc.second then cur_branch_search_end_inc
          else { cur_branch_search_end_inc with second = y }
        in
        (range_map_start, range_map_end_inc)
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      if Int_set.is_empty t.seconds then
        Seq.return
          (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
      else
        t.seconds
        |> Int_set.to_seq
        |> filter_seconds ~cur_branch_search_start ~cur_branch_search_end_inc
        |> Time.Second_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:
               (range_map_inc ~cur_branch_search_start
                  ~cur_branch_search_end_inc)
             ~f_exc:failwith_unexpected_case)
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

    let get_cur_branch_search_end_inc
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_end_inc.year
        && cur_branch.month = overall_search_end_inc.month
        && cur_branch.day = overall_search_end_inc.day
        && cur_branch.hour = overall_search_end_inc.hour
      then overall_search_end_inc
      else Time.Date_time.set_to_last_min_sec cur_branch

    let filter_minutes ~(cur_branch_search_start : Time.Date_time.t)
        ~(cur_branch_search_end_inc : Time.Date_time.t) s =
      Seq.filter
        (fun min ->
           cur_branch_search_start.minute <= min
           && min <= cur_branch_search_end_inc.minute)
        s

    let matching_minutes ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      if Int_set.is_empty t.minutes then
        Seq.map
          (fun minute -> { cur_branch with minute })
          OSeq.(
            cur_branch_search_start.minute -- cur_branch_search_end_inc.minute)
      else
        t.minutes
        |> Int_set.to_seq
        |> filter_minutes ~cur_branch_search_start ~cur_branch_search_end_inc
        |> Seq.map (fun minute -> { cur_branch_search_start with minute })

    let matching_minute_ranges ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t)
        (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t)
          ~(cur_branch_search_end_inc : Time.Date_time.t) (x, y) =
        let range_map_start =
          if x = cur_branch_search_start.minute then cur_branch_search_start
          else
            Time.Date_time.set_to_first_sec
              { cur_branch_search_start with minute = x }
        in
        let range_map_end_inc =
          if y = cur_branch_search_end_inc.minute then cur_branch_search_end_inc
          else
            Time.Date_time.set_to_last_sec
              { cur_branch_search_end_inc with minute = y }
        in
        (range_map_start, range_map_end_inc)
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      if Int_set.is_empty t.minutes then
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_min_sec cur_branch_search_start ))
      else
        t.minutes
        |> Int_set.to_seq
        |> filter_minutes ~cur_branch_search_start ~cur_branch_search_end_inc
        |> Time.Minute_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:
               (range_map_inc ~cur_branch_search_start
                  ~cur_branch_search_end_inc)
             ~f_exc:failwith_unexpected_case)
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

    let get_cur_branch_search_end_inc
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_end_inc.year
        && cur_branch.month = overall_search_end_inc.month
        && cur_branch.day = overall_search_end_inc.day
      then overall_search_end_inc
      else Time.Date_time.set_to_last_hour_min_sec cur_branch

    let filter_hours ~(cur_branch_search_start : Time.Date_time.t)
        ~(cur_branch_search_end_inc : Time.Date_time.t) s =
      Seq.filter
        (fun hour ->
           cur_branch_search_start.hour <= hour
           && hour <= cur_branch_search_end_inc.hour)
        s

    let matching_hours (t : Time.Pattern.t)
        ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      if Int_set.is_empty t.hours then
        Seq.map
          (fun hour -> { cur_branch with hour })
          OSeq.(cur_branch_search_start.hour -- cur_branch_search_end_inc.hour)
      else
        t.hours
        |> Int_set.to_seq
        |> filter_hours ~cur_branch_search_start ~cur_branch_search_end_inc
        |> Seq.map (fun hour -> { cur_branch with hour })

    let matching_hour_ranges ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t)
        (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t)
          ~(cur_branch_search_end_inc : Time.Date_time.t) (x, y) =
        let range_map_start =
          if x = cur_branch_search_start.hour then cur_branch_search_start
          else
            Time.Date_time.set_to_first_min_sec
              { cur_branch_search_start with hour = x }
        in
        let range_map_end_inc =
          if y = cur_branch_search_end_inc.hour then cur_branch_search_end_inc
          else
            Time.Date_time.set_to_last_min_sec
              { cur_branch_search_end_inc with hour = y }
        in
        (range_map_start, range_map_end_inc)
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      if Int_set.is_empty t.hours then
        Seq.return
          (`Range_inc
             ( cur_branch_search_start,
               Time.Date_time.set_to_last_hour_min_sec cur_branch ))
      else
        t.hours
        |> Int_set.to_seq
        |> filter_hours ~cur_branch_search_start ~cur_branch_search_end_inc
        |> Time.Hour_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:
               (range_map_inc ~cur_branch_search_start
                  ~cur_branch_search_end_inc)
             ~f_exc:failwith_unexpected_case)
  end

  module Matching_days = struct
    let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_start.year
        && cur_branch.month = overall_search_start.month
      then overall_search_start
      else Time.Date_time.set_to_first_day_hour_min_sec cur_branch

    let get_cur_branch_search_end_inc
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if
        cur_branch.year = overall_search_end_inc.year
        && cur_branch.month = overall_search_end_inc.month
      then overall_search_end_inc
      else Time.Date_time.set_to_last_day_hour_min_sec cur_branch

    let int_month_days_of_matching_weekdays
        ~(cur_branch_search_start : Time.Date_time.t)
        ~(cur_branch_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t) :
      int Seq.t =
      let day_count =
        Time.day_count_of_month ~year:cur_branch_search_start.year
          ~month:cur_branch_search_start.month
      in
      let day_end_inc = min day_count cur_branch_search_end_inc.day in
      if Time.Weekday_set.is_empty t.weekdays then
        OSeq.(cur_branch_search_start.day -- day_end_inc)
      else
        OSeq.(cur_branch_search_start.day -- day_end_inc)
        |> Seq.filter (fun mday ->
            match
              Time.weekday_of_month_day ~year:cur_branch_search_start.year
                ~month:cur_branch_search_start.month ~mday
            with
            | Ok wday -> Time.Weekday_set.mem wday t.weekdays
            | Error () -> false)

    let direct_matching_int_month_days
        ~(cur_branch_search_start : Time.Date_time.t)
        ~(cur_branch_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t) :
      int Seq.t =
      let day_count =
        Time.day_count_of_month ~year:cur_branch_search_start.year
          ~month:cur_branch_search_start.month
      in
      let day_end_inc = min day_count cur_branch_search_end_inc.day in
      if Int_set.is_empty t.month_days then
        OSeq.(cur_branch_search_start.day -- day_end_inc)
      else
        t.month_days
        |> Int_set.to_seq
        |> Seq.map (fun mday -> if mday < 0 then day_count + mday + 1 else mday)
        |> Seq.filter (fun mday ->
            cur_branch_search_start.day <= mday && mday <= day_end_inc)

    let matching_int_month_days ~(cur_branch_search_start : Time.Date_time.t)
        ~(cur_branch_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t) :
      int Seq.t =
      let matching_month_days =
        direct_matching_int_month_days t ~cur_branch_search_start
          ~cur_branch_search_end_inc
        |> Int_set.of_seq
      in
      let month_days_of_matching_weekdays =
        int_month_days_of_matching_weekdays t ~cur_branch_search_start
          ~cur_branch_search_end_inc
        |> Int_set.of_seq
      in
      Int_set.inter matching_month_days month_days_of_matching_weekdays
      |> Int_set.to_seq

    let matching_days ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      matching_int_month_days t ~cur_branch_search_start
        ~cur_branch_search_end_inc
      |> Seq.map (fun day -> { cur_branch_search_start with day })

    let matching_day_ranges ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t)
        (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t)
          ~(cur_branch_search_end_inc : Time.Date_time.t) (x, y) =
        let range_map_start =
          if x = cur_branch_search_start.day then cur_branch_search_start
          else
            Time.Date_time.set_to_first_hour_min_sec
              { cur_branch_search_start with day = x }
        in
        let range_map_end_inc =
          if y = cur_branch_search_end_inc.day then cur_branch_search_end_inc
          else
            Time.Date_time.set_to_last_hour_min_sec
              { cur_branch_search_end_inc with day = y }
        in
        (range_map_start, range_map_end_inc)
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      let f_inc =
        range_map_inc ~cur_branch_search_start ~cur_branch_search_end_inc
      in
      let f_exc = failwith_unexpected_case in
      match
        (Int_set.is_empty t.month_days, Time.Weekday_set.is_empty t.weekdays)
      with
      | true, true ->
        Seq.return
          (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
      | true, false ->
        int_month_days_of_matching_weekdays t ~cur_branch_search_start
          ~cur_branch_search_end_inc
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
      | false, true ->
        direct_matching_int_month_days t ~cur_branch_search_start
          ~cur_branch_search_end_inc
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
      | false, false ->
        matching_int_month_days t ~cur_branch_search_start
          ~cur_branch_search_end_inc
        |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
        |> Seq.map (Time.Range.map ~f_inc ~f_exc)
  end

  module Matching_months = struct
    let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if cur_branch.year = overall_search_start.year then overall_search_start
      else Time.Date_time.set_to_first_month_day_hour_min_sec cur_branch

    let get_cur_branch_search_end_inc
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t =
      if cur_branch.year = overall_search_end_inc.year then
        overall_search_end_inc
      else Time.Date_time.set_to_last_month_day_hour_min_sec cur_branch

    let matching_months (t : Time.Pattern.t)
        ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      let month_start_int =
        Time.human_int_of_month cur_branch_search_start.month
      in
      let month_end_inc_int =
        Time.human_int_of_month cur_branch_search_end_inc.month
      in
      if Time.Month_set.is_empty t.months then
        OSeq.(month_start_int -- month_end_inc_int)
        |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
        |> Seq.map (fun month -> { cur_branch_search_start with month })
      else
        t.months
        |> Time.Month_set.to_seq
        |> Seq.map Time.human_int_of_month
        |> Seq.filter (fun month ->
            month_start_int <= month && month <= month_end_inc_int)
        |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
        |> Seq.map (fun month -> { cur_branch_search_start with month })

    let matching_month_ranges (t : Time.Pattern.t)
        ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_inc ~(cur_branch_search_start : Time.Date_time.t)
          ~(cur_branch_search_end_inc : Time.Date_time.t) (x, y) =
        let range_map_start =
          if x = cur_branch_search_start.month then cur_branch_search_start
          else
            Time.Date_time.set_to_first_day_hour_min_sec
              { cur_branch_search_start with month = x }
        in
        let range_map_end_inc =
          if y = cur_branch_search_end_inc.month then cur_branch_search_end_inc
          else
            Time.Date_time.set_to_last_day_hour_min_sec
              { cur_branch_search_end_inc with month = y }
        in
        (range_map_start, range_map_end_inc)
      in
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      let cur_branch_search_end_inc =
        get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
      in
      let month_start_int =
        Time.human_int_of_month cur_branch_search_start.month
      in
      let month_end_inc_int =
        Time.human_int_of_month cur_branch_search_end_inc.month
      in
      if Time.Month_set.is_empty t.months then
        Seq.return
          (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
      else
        t.months
        |> Time.Month_set.to_seq
        |> Seq.map Time.human_int_of_month
        |> Seq.filter (fun month ->
            month_start_int <= month && month <= month_end_inc_int)
        |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
        |> Time.Month_ranges.Of_seq.range_seq_of_seq
        |> Seq.map
          (Time.Range.map
             ~f_inc:
               (range_map_inc ~cur_branch_search_start
                  ~cur_branch_search_end_inc)
             ~f_exc:failwith_unexpected_case)
  end

  module Matching_years = struct
    let matching_years ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t) :
      Time.Date_time.t Seq.t =
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

    let matching_year_ranges ~(overall_search_start : Time.Date_time.t)
        ~(overall_search_end_inc : Time.Date_time.t) (t : Time.Pattern.t) :
      Time.Date_time.t Time.Range.range Seq.t =
      let range_map_inc ~(overall_search_start : Time.Date_time.t)
          ~(overall_search_end_inc : Time.Date_time.t) (x, y) =
        let range_map_start =
          if x = overall_search_start.year then overall_search_start
          else
            Time.Date_time.set_to_first_month_day_hour_min_sec
              { overall_search_start with year = x }
        in
        let range_map_end_inc =
          if y = overall_search_end_inc.year then overall_search_end_inc
          else
            Time.Date_time.set_to_last_month_day_hour_min_sec
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

  let date_time_range_seq_of_timestamps ~search_using_tz (s : int64 Seq.t) :
    Time.Date_time.t Time.Range.range Seq.t =
    let f (x, y) =
      ( Time.Date_time.of_timestamp ~tz_of_date_time:search_using_tz x,
        Time.Date_time.of_timestamp ~tz_of_date_time:search_using_tz y )
    in
    s
    |> Time.Ranges.Of_seq.range_seq_of_seq ~modulo:None ~to_int64:Fun.id
      ~of_int64:Fun.id
    |> Seq.map (Time.Range.map ~f_inc:f ~f_exc:f)
    |> Seq.filter_map Time.Range_utils.result_range_get

  type error = Time.Pattern.error

  let matching_date_times (search_param : Search_param.t) (pat : Time.Pattern.t)
    : Time.Date_time.t Seq.t =
    let overall_search_start = search_param.start in
    let overall_search_end_inc = search_param.end_inc in
    Matching_years.matching_years ~overall_search_start ~overall_search_end_inc
      pat
    |> Seq.flat_map
      (Matching_months.matching_months pat ~overall_search_start
         ~overall_search_end_inc)
    |> Seq.flat_map
      (Matching_days.matching_days pat ~overall_search_start
         ~overall_search_end_inc)
    |> Seq.flat_map
      (Matching_hours.matching_hours pat ~overall_search_start
         ~overall_search_end_inc)
    |> Seq.flat_map
      (Matching_minutes.matching_minutes pat ~overall_search_start
         ~overall_search_end_inc)
    |> Seq.flat_map
      (Matching_seconds.matching_seconds pat ~overall_search_start
         ~overall_search_end_inc)

  let matching_date_time_ranges (search_param : Search_param.t)
      (t : Time.Pattern.t) : Time.Date_time.t Time.Range.range Seq.t =
    let overall_search_start = search_param.start in
    let overall_search_end_inc = search_param.end_inc in
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
      Matching_years.matching_year_ranges ~overall_search_start
        ~overall_search_end_inc t
    | _years_is_empty, _months_is_empty, true, true, true, true, true ->
      Matching_years.matching_years ~overall_search_start
        ~overall_search_end_inc t
      |> Seq.flat_map
        (Matching_months.matching_month_ranges t ~overall_search_start
           ~overall_search_end_inc)
    | ( _years_is_empty,
        _months_is_empty,
        _month_days_is_empty,
        _weekdays_is_empty,
        true,
        true,
        true ) ->
      Matching_years.matching_years ~overall_search_start
        ~overall_search_end_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_days.matching_day_ranges t ~overall_search_start
           ~overall_search_end_inc)
    | ( _years_is_empty,
        _months_is_empty,
        _month_days_is_empty,
        _weekdays_is_empty,
        _hours_is_empty,
        true,
        true ) ->
      Matching_years.matching_years ~overall_search_start
        ~overall_search_end_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_days.matching_days t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_hours.matching_hour_ranges t ~overall_search_start
           ~overall_search_end_inc)
    | ( _years_is_empty,
        _months_is_empty,
        _month_days_is_empty,
        _weekdays_is_empty,
        _hours_is_empty,
        _minutes_is_empty,
        true ) ->
      Matching_years.matching_years ~overall_search_start
        ~overall_search_end_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_days.matching_days t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_hours.matching_hours t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_minutes.matching_minute_ranges t ~overall_search_start
           ~overall_search_end_inc)
    | ( _years_is_empty,
        _months_is_empty,
        _month_days_is_empty,
        _weekdays_is_empty,
        _hours_is_empty,
        _minutes_is_empty,
        _seconds_is_empty ) ->
      Matching_years.matching_years ~overall_search_start
        ~overall_search_end_inc t
      |> Seq.flat_map
        (Matching_months.matching_months t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_days.matching_days t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_hours.matching_hours t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_minutes.matching_minutes t ~overall_search_start
           ~overall_search_end_inc)
      |> Seq.flat_map
        (Matching_seconds.matching_second_ranges t ~overall_search_start
           ~overall_search_end_inc)

  let matching_intervals (search_param : Search_param.t) (t : Time.Pattern.t) :
    (int64 * int64) Seq.t =
    let f (x, y) =
      let x = Time.Date_time.to_timestamp_exact x in
      let y = Time.Date_time.to_timestamp_exact y in
      (x, y)
    in
    matching_date_time_ranges search_param t
    |> Seq.map (Time.Range.map ~f_inc:f ~f_exc:f)
    |> Seq.map (fun r ->
        match r with
        | `Range_inc (x, y) -> (x, Int64.succ y)
        | `Range_exc (x, y) -> (x, y))
    |> Time.Intervals.normalize ~skip_filter_invalid:true ~skip_sort:true

  (* let matching_intervals_round_robin_non_decreasing
   *     (search_param : Search_param.t) (l : Time.Pattern.t list) :
   *   ((int64 * int64) list Seq.t, error) result =
   *   let l = List.map (matching_intervals search_param) l in
   *   l
   *   |> Time.Intervals.Round_robin.collect_round_robin_non_decreasing
   *     ~skip_check:false
   *   |> OSeq.take_while (List.for_all Option.is_some)
   *   |> Seq.map (List.map Option.get)
   *   |> Result.ok
   * 
   * let matching_intervals_round_robin_non_decreasing_flat
   *     (search_param : Search_param.t) (l : Time.Pattern.t list) :
   *   ((int64 * int64) Seq.t, error) result =
   *   matching_intervals_round_robin_non_decreasing search_param l
   *   |> Result.map (Seq.flat_map List.to_seq)
   * 
   * let next_match_date_time (search_param : Search_param.t) (t : Time.Pattern.t)
   *   : Time.Date_time.t option =
   *   let s = matching_date_times search_param t in
   *   match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x *)

  (* let next_match_timestamp (search_param : Search_param.t) (t : Time.Pattern.t)
   *   : int64 option =
   *   match next_match_date_time search_param t with
   *   | None -> None
   *   | Some x -> Some (Time.Date_time.to_timestamp x) *)

  (* let next_match_interval (search_param : Pattern_search_param.t) (t : Time.Pattern.t) :
   *   (int64 * int64) option =
   *   let s = matching_intervals search_param t in
   *   match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x *)
end

let rec get_search_space (time : Time.t) : Time.Interval.t list =
  let open Time in
  match time with
  | All -> default_search_space
  | Empty -> []
  | Timestamp_interval_seq (s, _) -> s
  | Pattern (s, _) -> s
  | Unary_op (s, _, _) -> s
  | Interval_exc (s, _, _) -> s
  | Interval_inc (s, _, _) -> s
  | Round_robin_pick_list (s, _) -> s
  | Inter_seq (s, _) -> s
  | Union_seq (s, _) -> s
  | After (s, _, _, _) -> s
  | Between_inc (s, _, _, _) -> s
  | Between_exc (s, _, _, _) -> s
  | Unchunk c -> get_search_space_chunked c

and get_search_space_chunked (chunked : Time.chunked) =
  let open Time in
  match chunked with
  | Unary_op_on_t (_, t) -> get_search_space t
  | Unary_op_on_chunked (_, c) -> get_search_space_chunked c

let search_space_of_year_range tz year_range =
  let open Time in
  let aux_start start =
    Date_time.set_to_first_month_day_hour_min_sec
      { Date_time.min with year = start; tz_info = `Tz_only tz }
    |> Date_time.to_timestamp
    |> Date_time.min_of_timestamp_local_result
    |> Option.get
  in
  let aux_end_inc end_exc =
    Date_time.set_to_last_month_day_hour_min_sec
      { Date_time.min with year = end_exc; tz_info = `Tz_only tz }
    |> Date_time.to_timestamp
    |> Date_time.min_of_timestamp_local_result
    |> Option.get
    |> Int64.succ
  in
  let aux_end_exc end_exc =
    Date_time.set_to_first_month_day_hour_min_sec
      { Date_time.min with year = end_exc; tz_info = `Tz_only tz }
    |> Date_time.to_timestamp
    |> Date_time.min_of_timestamp_local_result
    |> Option.get
  in
  match year_range with
  | `Range_inc (start, end_inc) -> (aux_start start, aux_end_inc end_inc)
  | `Range_exc (start, end_exc) -> (aux_start start, aux_end_exc end_exc)

let search_space_of_year tz_offset_s year =
  search_space_of_year_range tz_offset_s (`Range_inc (year, year))

let empty_search_space = []

let propagate_search_space_bottom_up default_tz (time : Time.t) : Time.t =
  let open Time in
  let rec aux (tz : Time_zone.t) time =
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
          |> Seq.map (search_space_of_year tz)
          |> List.of_seq
        in
        Pattern (space, pat)
    | Unary_op (_, op, t) -> (
        match op with
        | Not -> Unary_op (default_search_space, op, aux tz t)
        | With_tz tz ->
          let t = aux tz t in
          Unary_op (get_search_space t, op, t)
        | _ ->
          let t = aux tz t in
          Unary_op (get_search_space t, op, t))
    | Inter_seq (_, s) ->
      let s = Seq.map (aux tz) s in
      let space =
        s
        |> Seq.map get_search_space
        |> Seq.map List.to_seq
        |> Seq.map (Intervals.normalize ~skip_sort:true)
        |> Intervals.Inter.inter_multi_seq ~skip_check:false
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
      let space, l = aux_list tz l in
      Round_robin_pick_list (space, l)
    | Union_seq (_, s) ->
      let space, s = aux_seq tz s in
      Union_seq (space, s)
    | After (_, b, t1, t2) ->
      let space =
        Intervals.Union.union
          (List.to_seq @@ get_search_space t1)
          (List.to_seq @@ get_search_space t2)
        |> List.of_seq
      in
      After (space, b, t1, t2)
    | Between_inc (_, b, t1, t2) ->
      let space =
        Intervals.Union.union
          (List.to_seq @@ get_search_space t1)
          (List.to_seq @@ get_search_space t2)
        |> List.of_seq
      in
      Between_inc (space, b, t1, t2)
    | Between_exc (_, b, t1, t2) ->
      let space =
        Intervals.Union.union
          (List.to_seq @@ get_search_space t1)
          (List.to_seq @@ get_search_space t2)
        |> List.of_seq
      in
      Between_exc (space, b, t1, t2)
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
  aux default_tz time

let propagate_search_space_top_down (time : Time.t) : Time.t =
  let open Time in
  let restrict_search_space (parent : search_space) (cur : search_space) =
    Intervals.Inter.inter ~skip_check:false (List.to_seq parent)
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
    | Unary_op (cur, op, t) -> (
        let space = restrict_search_space parent_search_space cur in
        match op with
        | Shift n ->
          let space =
            List.map (fun (x, y) -> (Int64.sub x n, Int64.sub y n)) space
          in
          Unary_op (space, op, aux space t)
        | _ -> Unary_op (space, op, aux space t))
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
    | After (cur, b, t1, t2) ->
      let space = restrict_search_space parent_search_space cur in
      After (space, b, aux space t1, aux space t2)
    | Between_inc (cur, b, t1, t2) ->
      let space = restrict_search_space parent_search_space cur in
      Between_inc (space, b, aux space t1, aux space t2)
    | Between_exc (cur, b, t1, t2) ->
      let space = restrict_search_space parent_search_space cur in
      Between_exc (space, b, aux space t1, aux space t2)
    | Unchunk c -> Unchunk c
  and aux_list parent_search_space l = List.map (aux parent_search_space) l
  and aux_seq parent_search_space l = Seq.map (aux parent_search_space) l in
  aux default_search_space time

let optimize_search_space default_tz_offset_s t =
  t
  |> propagate_search_space_bottom_up default_tz_offset_s
  |> propagate_search_space_top_down

type inc_or_exc =
  | Inc
  | Exc

let do_drop_n_points (n : int64) (s : Time.Interval.t Seq.t) :
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

let find_after bound (s1 : Time.Interval.t Seq.t) (s2 : Time.Interval.t Seq.t) =
  s1
  |> Seq.filter_map (fun (_start, end_exc) ->
      match OSeq.drop_while (fun (start', _) -> start' < end_exc) s2 () with
      | Seq.Nil -> None
      | Seq.Cons ((start', end_exc'), _) ->
        if Int64.sub start' end_exc <= bound then Some (start', end_exc')
        else None)

let find_between_inc bound (s1 : Time.Interval.t Seq.t)
    (s2 : Time.Interval.t Seq.t) =
  let rec aux s1 s2 =
    match s1 () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start1, end_exc1), rest1) -> (
        let s2 = OSeq.drop_while (fun (start2, _) -> start2 < end_exc1) s2 in
        match s2 () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons ((start2, end_exc2), _rest2) ->
          if Int64.sub start2 end_exc1 <= bound then
            OSeq.cons (start1, end_exc2) (aux rest1 s2)
          else aux rest1 s2)
  in
  aux s1 s2

let find_between_exc bound (s1 : Time.Interval.t Seq.t)
    (s2 : Time.Interval.t Seq.t) =
  let rec aux s1 s2 =
    match s1 () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start1, end_exc1), rest1) -> (
        let s2 = OSeq.drop_while (fun (start2, _) -> start2 < end_exc1) s2 in
        match s2 () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons ((start2, _end_exc2), _rest2) ->
          if Int64.sub start2 end_exc1 <= bound then
            OSeq.cons (start1, start2) (aux rest1 s2)
          else aux rest1 s2)
  in
  aux s1 s2

let do_chunk_at_year_boundary tz (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        Result.get_ok @@ Date_time.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time.of_timestamp ~tz_of_date_time:tz
        |> Result.get_ok
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time.set_to_last_day_hour_min_sec dt1
          |> Date_time.to_timestamp
          |> Date_time.max_of_timestamp_local_result
          |> Option.get
          |> Int64.succ
        in
        OSeq.cons (t1, t') (aux (OSeq.cons (t', t2) rest))
  in
  aux s

let do_chunk_at_month_boundary tz (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        Result.get_ok @@ Date_time.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time.of_timestamp ~tz_of_date_time:tz
        |> Result.get_ok
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time.set_to_last_day_hour_min_sec dt1
          |> Date_time.to_timestamp
          |> Date_time.max_of_timestamp_local_result
          |> Option.get
          |> Int64.succ
        in
        OSeq.cons (t1, t') (aux (OSeq.cons (t', t2) rest))
  in
  aux s

let resolve ?(search_using_tz = Time_zone.utc) (time : Time.t) :
  (Time.Interval.t Seq.t, string) result =
  let open Time in
  let normalize =
    Intervals.normalize ~skip_filter_invalid:true ~skip_sort:true
  in
  let rec aux search_using_tz time =
    match time with
    | Empty -> Seq.empty
    | All -> Seq.return (min_timestamp, Int64.succ @@ max_timestamp)
    | Timestamp_interval_seq (_, s) -> s
    | Pattern (space, pat) ->
      Time_zone.transition_seq search_using_tz
      |> Seq.flat_map (fun ((x, y), entry) ->
          let space =
            Intervals.Inter.inter (Seq.return (x, y)) (List.to_seq space)
          in
          let params =
            Seq.map
              (Resolve_pattern.Search_param.make ~search_using_tz
                 ~search_using_tz_offset_s:Time_zone.(entry.offset))
              space
          in
          Intervals.Union.union_multi_seq ~skip_check:false
            (Seq.map
               (fun param -> Resolve_pattern.matching_intervals param pat)
               params))
      |> normalize
    | Unary_op (space, op, t) -> (
        let search_using_tz =
          match op with With_tz x -> x | _ -> search_using_tz
        in
        let s = aux search_using_tz t in
        match op with
        | Not ->
          Intervals.relative_complement ~skip_check:false ~not_mem_of:s
            (List.to_seq space)
        | Every -> s
        | Drop_n_points n -> do_drop_n_points (Int64.of_int n) s |> normalize
        | Take_n_points n -> do_take_n_points (Int64.of_int n) s
        | Shift n ->
          Seq.map
            (fun (start, end_exc) -> (Int64.add start n, Int64.add end_exc n))
            s
        | Lengthen n ->
          s
          |> Seq.map (fun (start, end_exc) -> (start, Int64.add end_exc n))
          |> normalize
        | With_tz _ -> s)
    | Interval_inc (_, a, b) -> Seq.return (a, Int64.succ b)
    | Interval_exc (_, a, b) -> Seq.return (a, b)
    | Round_robin_pick_list (_, l) ->
      List.map (aux search_using_tz) l
      |> Time.Intervals.Round_robin
         .merge_multi_list_round_robin_non_decreasing ~skip_check:false
    | Inter_seq (_, s) ->
      Seq.map (aux search_using_tz) s
      |> Time.Intervals.Inter.inter_multi_seq ~skip_check:false
    | Union_seq (_, s) ->
      Seq.map (aux search_using_tz) s
      |> Time.Intervals.Union.union_multi_seq ~skip_check:false
    | After (_, b, t1, t2) ->
      let s1 = aux search_using_tz t1 in
      let s2 = aux search_using_tz t2 in
      find_after b s1 s2
    | Between_inc (_, b, t1, t2) ->
      let s1 = aux search_using_tz t1 in
      let s2 = aux search_using_tz t2 in
      find_between_inc b s1 s2
    | Between_exc (_, b, t1, t2) ->
      let s1 = aux search_using_tz t1 in
      let s2 = aux search_using_tz t2 in
      find_between_exc b s1 s2
    | Unchunk c -> aux_chunked search_using_tz c |> normalize
  and aux_chunked search_using_tz_offset_s (chunked : chunked) =
    let chunk_based_on_op_on_t op s =
      match op with
      | Chunk_disjoint_interval ->
        normalize s
      | Chunk_by_duration { chunk_size; drop_partial } ->
        Intervals.chunk ~skip_check:false ~drop_partial ~chunk_size s
      | Chunk_at_year_boundary ->
        do_chunk_at_year_boundary search_using_tz_offset_s s
      | Chunk_at_month_boundary ->
        do_chunk_at_month_boundary search_using_tz_offset_s s
    in
    match chunked with
    | Unary_op_on_t (op, t) ->
      aux search_using_tz_offset_s t |> chunk_based_on_op_on_t op
    | Unary_op_on_chunked (op, c) -> (
        let s = aux_chunked search_using_tz_offset_s c in
        match op with
        | Nth n -> s |> OSeq.drop n |> OSeq.take 1
        | Drop n -> OSeq.drop n s
        | Take n -> OSeq.take n s
        | Take_nth n -> OSeq.take_nth n s
        | Chunk_again op -> chunk_based_on_op_on_t op s)
  in
  try
    time
    |> optimize_search_space search_using_tz
    |> aux search_using_tz
    |> normalize
    |> Result.ok
  with
  | Interval_is_invalid -> Error "Invalid interval"
  | Intervals_are_not_sorted -> Error "Intervals are not sorted"
