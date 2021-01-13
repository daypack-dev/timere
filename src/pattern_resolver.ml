open Date_components

module Search_param = struct
  type t = {
    search_using_tz_offset_s : int;
    start : Time.Date_time'.t;
    end_inc : Time.Date_time'.t;
  }

  let make ~search_using_tz_offset_s ~search_using_tz
      ((start, end_exc) : Time.Interval.t) : t =
    {
      search_using_tz_offset_s;
      start =
        CCResult.get_exn
        @@ Time.Date_time'.of_timestamp ~tz_of_date_time:search_using_tz start;
      end_inc =
        CCResult.get_exn
        @@ Time.Date_time'.of_timestamp ~tz_of_date_time:search_using_tz
          (Int64.pred end_exc);
    }
end

let failwith_unexpected_case (_ : 'a) = failwith "Unexpected case"

module Matching_seconds = struct
  let get_cur_branch_search_start ~(overall_search_start : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if
      cur_branch.year = overall_search_start.year
      && cur_branch.month = overall_search_start.month
      && cur_branch.day = overall_search_start.day
      && cur_branch.hour = overall_search_start.hour
      && cur_branch.minute = overall_search_start.minute
    then overall_search_start
    else Time.Date_time'.set_to_first_sec cur_branch

  let get_cur_branch_search_end_inc
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if
      cur_branch.year = overall_search_end_inc.year
      && cur_branch.month = overall_search_end_inc.month
      && cur_branch.day = overall_search_end_inc.day
      && cur_branch.hour = overall_search_end_inc.hour
      && cur_branch.minute = overall_search_end_inc.minute
    then overall_search_end_inc
    else Time.Date_time'.set_to_last_sec cur_branch

  let filter_seconds ~(cur_branch_search_start : Time.Date_time'.t)
      ~(cur_branch_search_end_inc : Time.Date_time'.t) s =
    Seq.filter
      (fun sec ->
         cur_branch_search_start.second <= sec
         && sec <= cur_branch_search_end_inc.second)
      s

  let matching_seconds ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t) (t : Pattern.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t Seq.t =
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

  let matching_second_ranges (t : Pattern.t)
      ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let range_map_inc ~(cur_branch_search_start : Time.Date_time'.t)
        ~(cur_branch_search_end_inc : Time.Date_time'.t) (x, y) =
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
  let get_cur_branch_search_start ~(overall_search_start : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if
      cur_branch.year = overall_search_start.year
      && cur_branch.month = overall_search_start.month
      && cur_branch.day = overall_search_start.day
      && cur_branch.hour = overall_search_start.hour
    then overall_search_start
    else Time.Date_time'.set_to_first_min_sec cur_branch

  let get_cur_branch_search_end_inc
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if
      cur_branch.year = overall_search_end_inc.year
      && cur_branch.month = overall_search_end_inc.month
      && cur_branch.day = overall_search_end_inc.day
      && cur_branch.hour = overall_search_end_inc.hour
    then overall_search_end_inc
    else Time.Date_time'.set_to_last_min_sec cur_branch

  let filter_minutes ~(cur_branch_search_start : Time.Date_time'.t)
      ~(cur_branch_search_end_inc : Time.Date_time'.t) s =
    Seq.filter
      (fun min ->
         cur_branch_search_start.minute <= min
         && min <= cur_branch_search_end_inc.minute)
      s

  let matching_minutes ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t) (t : Pattern.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t Seq.t =
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

  let matching_minute_ranges ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t) (t : Pattern.t)
      (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let range_map_inc ~(cur_branch_search_start : Time.Date_time'.t)
        ~(cur_branch_search_end_inc : Time.Date_time'.t) (x, y) =
      let range_map_start =
        if x = cur_branch_search_start.minute then cur_branch_search_start
        else
          Time.Date_time'.set_to_first_sec
            { cur_branch_search_start with minute = x }
      in
      let range_map_end_inc =
        if y = cur_branch_search_end_inc.minute then cur_branch_search_end_inc
        else
          Time.Date_time'.set_to_last_sec
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
             Time.Date_time'.set_to_last_min_sec cur_branch_search_start ))
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
  let get_cur_branch_search_start ~(overall_search_start : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if
      cur_branch.year = overall_search_start.year
      && cur_branch.month = overall_search_start.month
      && cur_branch.day = overall_search_start.day
    then overall_search_start
    else Time.Date_time'.set_to_first_hour_min_sec cur_branch

  let get_cur_branch_search_end_inc
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if
      cur_branch.year = overall_search_end_inc.year
      && cur_branch.month = overall_search_end_inc.month
      && cur_branch.day = overall_search_end_inc.day
    then overall_search_end_inc
    else Time.Date_time'.set_to_last_hour_min_sec cur_branch

  let filter_hours ~(cur_branch_search_start : Time.Date_time'.t)
      ~(cur_branch_search_end_inc : Time.Date_time'.t) s =
    Seq.filter
      (fun hour ->
         cur_branch_search_start.hour <= hour
         && hour <= cur_branch_search_end_inc.hour)
      s

  let matching_hours (t : Pattern.t) ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t Seq.t =
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

  let matching_hour_ranges ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t) (t : Pattern.t)
      (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let range_map_inc ~(cur_branch_search_start : Time.Date_time'.t)
        ~(cur_branch_search_end_inc : Time.Date_time'.t) (x, y) =
      let range_map_start =
        if x = cur_branch_search_start.hour then cur_branch_search_start
        else
          Time.Date_time'.set_to_first_min_sec
            { cur_branch_search_start with hour = x }
      in
      let range_map_end_inc =
        if y = cur_branch_search_end_inc.hour then cur_branch_search_end_inc
        else
          Time.Date_time'.set_to_last_min_sec
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
             Time.Date_time'.set_to_last_hour_min_sec cur_branch ))
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
  let get_cur_branch_search_start ~(overall_search_start : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if
      cur_branch.year = overall_search_start.year
      && cur_branch.month = overall_search_start.month
    then overall_search_start
    else Time.Date_time'.set_to_first_day_hour_min_sec cur_branch

  let get_cur_branch_search_end_inc
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if
      cur_branch.year = overall_search_end_inc.year
      && cur_branch.month = overall_search_end_inc.month
    then overall_search_end_inc
    else Time.Date_time'.set_to_last_day_hour_min_sec cur_branch

  let int_month_days_of_matching_weekdays
      ~(cur_branch_search_start : Time.Date_time'.t)
      ~(cur_branch_search_end_inc : Time.Date_time'.t) (t : Pattern.t) :
    int Seq.t =
    let day_count =
      day_count_of_month ~year:cur_branch_search_start.year
        ~month:cur_branch_search_start.month
    in
    let day_end_inc = min day_count cur_branch_search_end_inc.day in
    if Weekday_set.is_empty t.weekdays then
      OSeq.(cur_branch_search_start.day -- day_end_inc)
    else
      OSeq.(cur_branch_search_start.day -- day_end_inc)
      |> Seq.filter (fun mday ->
          match
            weekday_of_month_day ~year:cur_branch_search_start.year
              ~month:cur_branch_search_start.month ~mday
          with
          | Ok wday -> Weekday_set.mem wday t.weekdays
          | Error () -> false)

  let direct_matching_int_month_days
      ~(cur_branch_search_start : Time.Date_time'.t)
      ~(cur_branch_search_end_inc : Time.Date_time'.t) (t : Pattern.t) :
    int Seq.t =
    let day_count =
      day_count_of_month ~year:cur_branch_search_start.year
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

  let matching_int_month_days ~(cur_branch_search_start : Time.Date_time'.t)
      ~(cur_branch_search_end_inc : Time.Date_time'.t) (t : Pattern.t) :
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

  let matching_days ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t) (t : Pattern.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t Seq.t =
    let cur_branch_search_start =
      get_cur_branch_search_start ~overall_search_start cur_branch
    in
    let cur_branch_search_end_inc =
      get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
    in
    matching_int_month_days t ~cur_branch_search_start
      ~cur_branch_search_end_inc
    |> Seq.map (fun day -> { cur_branch_search_start with day })

  let matching_day_ranges ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t) (t : Pattern.t)
      (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let range_map_inc ~(cur_branch_search_start : Time.Date_time'.t)
        ~(cur_branch_search_end_inc : Time.Date_time'.t) (x, y) =
      let range_map_start =
        if x = cur_branch_search_start.day then cur_branch_search_start
        else
          Time.Date_time'.set_to_first_hour_min_sec
            { cur_branch_search_start with day = x }
      in
      let range_map_end_inc =
        if y = cur_branch_search_end_inc.day then cur_branch_search_end_inc
        else
          Time.Date_time'.set_to_last_hour_min_sec
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
    match (Int_set.is_empty t.month_days, Weekday_set.is_empty t.weekdays) with
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
  let get_cur_branch_search_start ~(overall_search_start : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if cur_branch.year = overall_search_start.year then overall_search_start
    else Time.Date_time'.set_to_first_month_day_hour_min_sec cur_branch

  let get_cur_branch_search_end_inc
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t =
    if cur_branch.year = overall_search_end_inc.year then overall_search_end_inc
    else Time.Date_time'.set_to_last_month_day_hour_min_sec cur_branch

  let matching_months (t : Pattern.t)
      ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) : Time.Date_time'.t Seq.t =
    let cur_branch_search_start =
      get_cur_branch_search_start ~overall_search_start cur_branch
    in
    let cur_branch_search_end_inc =
      get_cur_branch_search_end_inc ~overall_search_end_inc cur_branch
    in
    let month_start_int = human_int_of_month cur_branch_search_start.month in
    let month_end_inc_int =
      human_int_of_month cur_branch_search_end_inc.month
    in
    if Month_set.is_empty t.months then
      OSeq.(month_start_int -- month_end_inc_int)
      |> Seq.map (fun month -> month_of_human_int month |> CCResult.get_exn)
      |> Seq.map (fun month -> { cur_branch_search_start with month })
    else
      t.months
      |> Month_set.to_seq
      |> Seq.map human_int_of_month
      |> Seq.filter (fun month ->
          month_start_int <= month && month <= month_end_inc_int)
      |> Seq.map (fun month -> month_of_human_int month |> CCResult.get_exn)
      |> Seq.map (fun month -> { cur_branch_search_start with month })

  let matching_month_ranges (t : Pattern.t)
      ~(overall_search_start : Time.Date_time'.t)
      ~(overall_search_end_inc : Time.Date_time'.t)
      (cur_branch : Time.Date_time'.t) :
    Time.Date_time'.t Time.Range.range Seq.t =
    let range_map_inc ~(cur_branch_search_start : Time.Date_time'.t)
        ~(cur_branch_search_end_inc : Time.Date_time'.t) (x, y) =
      let range_map_start =
        if x = cur_branch_search_start.month then cur_branch_search_start
        else
          Time.Date_time'.set_to_first_day_hour_min_sec
            { cur_branch_search_start with month = x }
      in
      let range_map_end_inc =
        if y = cur_branch_search_end_inc.month then cur_branch_search_end_inc
        else
          Time.Date_time'.set_to_last_day_hour_min_sec
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
      |> Seq.map (fun month -> month_of_human_int month |> CCResult.get_exn)
      |> Time.Month_ranges.Of_seq.range_seq_of_seq
      |> Seq.map
        (Time.Range.map
           ~f_inc:
             (range_map_inc ~cur_branch_search_start
                ~cur_branch_search_end_inc)
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
        if x = overall_search_start.year then overall_search_start
        else
          Time.Date_time'.set_to_first_month_day_hour_min_sec
            { overall_search_start with year = x }
      in
      let range_map_end_inc =
        if y = overall_search_end_inc.year then overall_search_end_inc
        else
          Time.Date_time'.set_to_last_month_day_hour_min_sec
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
  Time.Date_time'.t Time.Range.range Seq.t =
  let f (x, y) =
    ( Time.Date_time'.of_timestamp ~tz_of_date_time:search_using_tz x,
      Time.Date_time'.of_timestamp ~tz_of_date_time:search_using_tz y )
  in
  s
  |> Time.Ranges.Of_seq.range_seq_of_seq ~modulo:None ~to_int64:CCFun.id
    ~of_int64:CCFun.id
  |> Seq.map (Time.Range.map ~f_inc:f ~f_exc:f)
  |> Seq.filter_map Time.Range_utils.result_range_get

type error = Pattern.error

let matching_date_times (search_param : Search_param.t) (pat : Pattern.t) :
  Time.Date_time'.t Seq.t =
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

let matching_date_time_ranges (search_param : Search_param.t) (t : Pattern.t) :
  Time.Date_time'.t Time.Range.range Seq.t =
  let overall_search_start = search_param.start in
  let overall_search_end_inc = search_param.end_inc in
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

let resolve (search_param : Search_param.t) (t : Pattern.t) :
  (int64 * int64) Seq.t =
  let f (x, y) =
    let x = Time.Date_time'.to_timestamp_single x in
    let y = Time.Date_time'.to_timestamp_single y in
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
 *     (search_param : Search_param.t) (l : Pattern.t list) :
 *   ((int64 * int64) list Seq.t, error) result =
 *   let l = List.map (matching_intervals search_param) l in
 *   l
 *   |> Time.Intervals.Round_robin.collect_round_robin_non_decreasing
 *     ~skip_check:false
 *   |> OSeq.take_while (List.for_all CCOpt.is_some)
 *   |> Seq.map (List.map CCOpt.get)
 *   |> CCResult.ok
 * 
 * let matching_intervals_round_robin_non_decreasing_flat
 *     (search_param : Search_param.t) (l : Pattern.t list) :
 *   ((int64 * int64) Seq.t, error) result =
 *   matching_intervals_round_robin_non_decreasing search_param l
 *   |> CCResult.map (Seq.flat_map CCList.to_seq)
 * 
 * let next_match_date_time (search_param : Search_param.t) (t : Pattern.t)
 *   : Time.Date_time'.t option =
 *   let s = matching_date_times search_param t in
 *   match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x *)

(* let next_match_timestamp (search_param : Search_param.t) (t : Pattern.t)
 *   : int64 option =
 *   match next_match_date_time search_param t with
 *   | None -> None
 *   | Some x -> Some (Time.Date_time'.to_timestamp x) *)

(* let next_match_interval (search_param : Pattern_search_param.t) (t : Pattern.t) :
 *   (int64 * int64) option =
 *   let s = matching_intervals search_param t in
 *   match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x *)
