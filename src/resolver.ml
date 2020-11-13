module Search_param = struct
  type typ =
    | Intervals of Time.Interval.t list
    | Years_ahead of {
        start : Time.Date_time.t;
        years_ahead : int;
      }

  type t = {
    search_using_tz_offset_s : Time.tz_offset_s option;
    typ : typ;
  }

  type error =
    | Invalid_start
    | Invalid_intervals
    | Invalid_search_years_ahead
    | Too_far_into_future

  let push_search_param_to_later_start ~(start : int64) (search_param : t) :
    (t, unit) result =
    match search_param.typ with
    | Intervals intervals -> (
        match Time.Intervals.Bound.min_start_and_max_end_exc_list intervals with
        | None -> Ok search_param
        | Some (start', end_exc') ->
          let start = max start' start in
          let intervals =
            intervals
            |> List.to_seq
            |> Time.Intervals.inter (Seq.return (start, end_exc'))
            |> List.of_seq
          in
          Ok { search_param with typ = Intervals intervals } )
    | Years_ahead { years_ahead; start = start' } -> (
        match Time.Date_time.to_timestamp start' with
        | Error () -> Error ()
        | Ok start' ->
          let start = max start' start in
          Time.Date_time.of_timestamp
            ~tz_offset_s_of_date_time:search_param.search_using_tz_offset_s
            start
          |> Result.map (fun start ->
              {
                search_param with
                typ = Years_ahead { years_ahead; start };
              }) )

  let start_date_time_and_search_years_ahead_of_search_param (search_param : t)
    : (Time.Date_time.t * int) option =
    match search_param.typ with
    | Intervals intervals -> (
        match Time.Intervals.Bound.min_start_and_max_end_exc_list intervals with
        | None -> None
        | Some (start, end_exc) ->
          let start =
            Time.Date_time.of_timestamp
              ~tz_offset_s_of_date_time:search_param.search_using_tz_offset_s
              start
            |> Result.get_ok
          in
          let end_exc =
            Time.Date_time.of_timestamp
              ~tz_offset_s_of_date_time:search_param.search_using_tz_offset_s
              end_exc
            |> Result.get_ok
          in
          let search_years_ahead = end_exc.year - start.year + 1 in
          Some (start, search_years_ahead) )
    | Years_ahead { years_ahead; start } -> Some (start, years_ahead)

  module Check = struct
    let check_search_param (x : t) : (unit, error) result =
      match x.typ with
      | Intervals intervals ->
        if
          List.for_all
            (fun (x, y) ->
               Time.Interval.Check.is_valid (x, y)
               && Time.Date_time.of_timestamp ~tz_offset_s_of_date_time:None x
                  |> Result.is_ok
               && Time.Date_time.of_timestamp ~tz_offset_s_of_date_time:None y
                  |> Result.is_ok)
            intervals
        then Ok ()
        else Error Invalid_intervals
      | Years_ahead { years_ahead; start } ->
        if Time.Check.date_time_is_valid start then
          if years_ahead <= 0 then Error Invalid_search_years_ahead
          else if start.year + years_ahead > Time.Date_time.max.year then
            Error Too_far_into_future
          else Ok ()
        else Error Invalid_start
  end
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

    let matching_seconds (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
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

    let matching_minutes (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
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

    let matching_minute_ranges (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) :
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

    let matching_hour_ranges (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) :
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

    let int_month_days_of_matching_weekdays (t : Time.Pattern.pattern)
        ~(cur_branch_search_start : Time.Date_time.t) : int Seq.t =
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

    let direct_matching_int_month_days (t : Time.Pattern.pattern)
        ~(cur_branch_search_start : Time.Date_time.t) : int Seq.t =
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

    let matching_int_month_days (t : Time.Pattern.pattern)
        ~(cur_branch_search_start : Time.Date_time.t) : int Seq.t =
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

    let matching_days (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) : Time.Date_time.t Seq.t =
      let cur_branch_search_start =
        get_cur_branch_search_start ~overall_search_start cur_branch
      in
      matching_int_month_days t ~cur_branch_search_start
      |> Seq.map (fun day -> { cur_branch_search_start with day })

    let matching_day_ranges (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t)
        (cur_branch : Time.Date_time.t) :
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
    let matching_years ~search_years_ahead (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t) : Time.Date_time.t Seq.t =
      match t.years with
      | [] ->
        OSeq.(
          overall_search_start.year
          --^ (overall_search_start.year + search_years_ahead))
        |> Seq.map (fun year -> { overall_search_start with year })
      | pat_year_list ->
        pat_year_list
        |> List.to_seq
        |> Seq.filter (fun year ->
            overall_search_start.year <= year
            && year < overall_search_start.year + search_years_ahead)
        |> Seq.map (fun year -> { overall_search_start with year })

    let matching_year_ranges ~search_years_ahead (t : Time.Pattern.pattern)
        ~(overall_search_start : Time.Date_time.t) :
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
                 {
                   overall_search_start with
                   year = overall_search_start.year + search_years_ahead - 1;
                 } ))
      | l ->
        List.sort_uniq compare l
        |> List.to_seq
        |> Seq.filter (fun year ->
            overall_search_start.year <= year
            && year < overall_search_start.year + search_years_ahead)
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

  type error =
    | Invalid_search_param of Search_param.error
    | Invalid_time_pattern of Time.Pattern.error

  let check_search_param_and_time_pattern (search_param : Search_param.t)
      (x : Time.Pattern.pattern) : (unit, error) result =
    match Search_param.Check.check_search_param search_param with
    | Error e -> Error (Invalid_search_param e)
    | Ok () -> (
        match Time.Pattern.Check.check_pattern x with
        | Error e -> Error (Invalid_time_pattern e)
        | Ok () -> Ok () )

  let override_search_param_possibly ~allow_search_param_override
      (search_param : Search_param.t) (t : Time.Pattern.pattern) :
    Search_param.t =
    if allow_search_param_override then
      match t.years with
      | [] -> search_param
      | l -> (
          let l = List.sort_uniq compare l in
          let start_year = List.hd l in
          let end_inc_year = List.hd (List.rev l) in
          let search_using_tz_offset_s =
            Option.value ~default:0 search_param.search_using_tz_offset_s
          in
          let start_date_time =
            let open Time.Date_time in
            {
              min with
              year = start_year;
              tz_offset_s = search_using_tz_offset_s;
            }
            |> set_to_first_month_day_hour_min_sec
          in
          match
            Search_param.start_date_time_and_search_years_ahead_of_search_param
              search_param
          with
          | None ->
            let open Search_param in
            {
              search_using_tz_offset_s = Some search_using_tz_offset_s;
              typ =
                Years_ahead
                  {
                    start = start_date_time;
                    years_ahead = end_inc_year - start_year + 1;
                  };
            }
          | Some (start_date_time', search_years_ahead') ->
            let cmp_value =
              Time.Date_time.compare start_date_time start_date_time'
            in
            let end_inc_year =
              max (start_date_time'.year + search_years_ahead') end_inc_year
            in
            let start_date_time =
              if cmp_value <= 0 then start_date_time else start_date_time'
            in
            let open Search_param in
            {
              search_using_tz_offset_s = Some search_using_tz_offset_s;
              typ =
                Years_ahead
                  {
                    start = start_date_time;
                    years_ahead = end_inc_year - start_date_time.year + 1;
                  };
            } )
    else search_param

  let matching_date_times ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (pat : Time.Pattern.pattern) :
    (Time.Date_time.t Seq.t, error) result =
    check_search_param_and_time_pattern search_param pat
    |> Result.map (fun () ->
        let search_param =
          override_search_param_possibly ~allow_search_param_override
            search_param pat
        in
        match
          Search_param.start_date_time_and_search_years_ahead_of_search_param
            search_param
        with
        | None -> Seq.empty
        | Some (overall_search_start, search_years_ahead) ->
          let search_using_tz_offset_s =
            search_param.search_using_tz_offset_s
          in
          Matching_years.matching_years ~search_years_ahead pat
            ~overall_search_start
          |> Seq.flat_map
            (Matching_months.matching_months pat ~overall_search_start)
          |> Seq.flat_map
            (Matching_days.matching_days pat ~overall_search_start)
          |> Seq.flat_map
            (Matching_hours.matching_hours pat ~overall_search_start)
          |> Seq.flat_map
            (Matching_minutes.matching_minutes pat
               ~overall_search_start)
          |> Seq.flat_map
            (Matching_seconds.matching_seconds pat
               ~overall_search_start)
          |> filter_using_matching_timestamps ~search_using_tz_offset_s pat
            ~overall_search_start)

  let matching_timestamps ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : Time.Pattern.pattern) :
    (int64 Seq.t, error) result =
    matching_date_times ~allow_search_param_override search_param t
    |> Result.map (fun s ->
        Seq.filter_map
          (fun x ->
             match Time.Date_time.to_timestamp x with
             | Ok x -> Some x
             | Error () -> None)
          s)

  let matching_date_time_ranges ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : Time.Pattern.pattern) :
    (Time.Date_time.t Time.Range.range Seq.t, error) result =
    match check_search_param_and_time_pattern search_param t with
    | Error msg -> Error msg
    | Ok () -> (
        let search_param =
          override_search_param_possibly ~allow_search_param_override
            search_param t
        in
        match
          Search_param.start_date_time_and_search_years_ahead_of_search_param
            search_param
        with
        | None -> Ok Seq.empty
        | Some (overall_search_start, search_years_ahead) -> (
            let search_using_tz_offset_s =
              search_param.search_using_tz_offset_s
            in
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
              Matching_years.matching_year_ranges ~search_years_ahead t
                ~overall_search_start
              |> Result.ok
            | _years, _months, [], [], [], [], [], [] ->
              Matching_years.matching_years ~search_years_ahead t
                ~overall_search_start
              |> Seq.flat_map
                (Matching_months.matching_month_ranges t
                   ~overall_search_start)
              |> Result.ok
            | _years, _months, _month_days, _weekdays, [], [], [], [] ->
              Matching_years.matching_years ~search_years_ahead t
                ~overall_search_start
              |> Seq.flat_map
                (Matching_months.matching_months t ~overall_search_start)
              |> Seq.flat_map
                (Matching_days.matching_day_ranges t ~overall_search_start)
              |> Result.ok
            | _years, _months, _month_days, _weekdays, _hours, [], [], [] ->
              Matching_years.matching_years ~search_years_ahead t
                ~overall_search_start
              |> Seq.flat_map
                (Matching_months.matching_months t ~overall_search_start)
              |> Seq.flat_map
                (Matching_days.matching_days t ~overall_search_start)
              |> Seq.flat_map
                (Matching_hours.matching_hour_ranges t
                   ~overall_search_start)
              |> Result.ok
            | _years, _months, _month_days, _weekdays, _hours, _minutes, [], []
              ->
              Matching_years.matching_years ~search_years_ahead t
                ~overall_search_start
              |> Seq.flat_map
                (Matching_months.matching_months t ~overall_search_start)
              |> Seq.flat_map
                (Matching_days.matching_days t ~overall_search_start)
              |> Seq.flat_map
                (Matching_hours.matching_hours t ~overall_search_start)
              |> Seq.flat_map
                (Matching_minutes.matching_minute_ranges t
                   ~overall_search_start)
              |> Result.ok
            | ( _years,
                _months,
                _month_days,
                _weekdays,
                _hours,
                _minutes,
                _seconds,
                [] ) ->
              Matching_years.matching_years ~search_years_ahead t
                ~overall_search_start
              |> Seq.flat_map
                (Matching_months.matching_months t ~overall_search_start)
              |> Seq.flat_map
                (Matching_days.matching_days t ~overall_search_start)
              |> Seq.flat_map
                (Matching_hours.matching_hours t ~overall_search_start)
              |> Seq.flat_map
                (Matching_minutes.matching_minutes t ~overall_search_start)
              |> Seq.flat_map
                (Matching_seconds.matching_second_ranges t
                   ~overall_search_start)
              |> Result.ok
            | [], [], [], [], [], [], [], timestamps ->
              timestamps
              |> List.to_seq
              |> date_time_range_seq_of_timestamps ~search_using_tz_offset_s
              |> Result.ok
            | ( _years,
                _months,
                _month_days,
                _weekdays,
                _hours,
                _minutes,
                _seconds,
                _timestamps ) ->
              Matching_years.matching_years ~search_years_ahead t
                ~overall_search_start
              |> Seq.flat_map
                (Matching_months.matching_months t ~overall_search_start)
              |> Seq.flat_map
                (Matching_days.matching_days t ~overall_search_start)
              |> Seq.flat_map
                (Matching_hours.matching_hours t ~overall_search_start)
              |> Seq.flat_map
                (Matching_minutes.matching_minutes t ~overall_search_start)
              |> Seq.flat_map
                (Matching_seconds.matching_seconds t ~overall_search_start)
              |> filter_using_matching_timestamps ~search_using_tz_offset_s t
                ~overall_search_start
              |> Seq.map (fun x -> `Range_inc (x, x))
              |> Result.ok ) )

  let matching_intervals ~allow_search_param_override
      (search_param : Search_param.t) (t : Time.Pattern.pattern) :
    ((int64 * int64) Seq.t, error) result =
    let f (x, y) =
      (Time.Date_time.to_timestamp x, Time.Date_time.to_timestamp y)
    in
    matching_date_time_ranges ~allow_search_param_override search_param t
    |> Result.map (fun s ->
        s
        |> Seq.map (Time.Range.map ~f_inc:f ~f_exc:f)
        |> Seq.filter_map Time.Range_utils.result_range_get
        |> Seq.map (fun r ->
            match r with
            | `Range_inc (x, y) -> (x, Int64.succ y)
            | `Range_exc (x, y) -> (x, y))
        |> fun l ->
        let intervals =
          match search_param.typ with
          | Intervals intervals ->
            Some
              ( intervals
                |> Time.Intervals.Normalize.normalize_list_in_seq_out )
          | _ -> None
        in
        match intervals with
        | None -> l
        | Some intervals ->
          Time.Intervals.inter intervals ~skip_check:true l
          |> Time.Intervals.Normalize.normalize ~skip_filter_invalid:true
            ~skip_sort:true)

  let matching_intervals_round_robin_non_decreasing ~allow_search_param_override
      (search_param : Search_param.t) (l : Time.Pattern.pattern list) :
    ((int64 * int64) list Seq.t, error) result =
    let l =
      List.map (matching_intervals ~allow_search_param_override search_param) l
    in
    match List.find_opt Result.is_error l with
    | Some e -> Error (Result.get_error e)
    | None ->
      l
      |> List.map Result.get_ok
      |> Time.Intervals.Round_robin.collect_round_robin_non_decreasing
        ~skip_check:true
      |> OSeq.take_while (List.for_all Option.is_some)
      |> Seq.map (List.map Option.get)
      |> Result.ok

  let matching_intervals_round_robin_non_decreasing_flat
      ~allow_search_param_override (search_param : Search_param.t)
      (l : Time.Pattern.pattern list) : ((int64 * int64) Seq.t, error) result =
    matching_intervals_round_robin_non_decreasing ~allow_search_param_override
      search_param l
    |> Result.map (Seq.flat_map List.to_seq)

  let next_match_date_time ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : Time.Pattern.pattern) :
    (Time.Date_time.t option, error) result =
    matching_date_times ~allow_search_param_override search_param t
    |> Result.map (fun s ->
        match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x)

  let next_match_timestamp ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : Time.Pattern.pattern) :
    (int64 option, error) result =
    next_match_date_time ~allow_search_param_override search_param t
    |> Result.map (fun x ->
        match x with
        | None -> None
        | Some x -> (
            match Time.Date_time.to_timestamp x with
            | Error () -> None
            | Ok x -> Some x ))

  let next_match_interval ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : Time.Pattern.pattern) :
    ((int64 * int64) option, error) result =
    matching_intervals ~allow_search_param_override search_param t
    |> Result.map (fun s ->
        match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x)
end

let resolve (search_param : Search_param.t) (time : Time.t) :
  ((int64 * int64) Seq.t, string) result =
  let rec aux search_param time =
    let open Time in
    match time with
    | Timestamp_interval_seq s -> Ok s
    | Pattern pat ->
      Resolve_pattern.matching_intervals ~allow_search_param_override:true
        search_param pat
      |> Result.map_error (fun _ -> "Error during resolution of pattern")
    | Branching branching -> failwith "Unimplemented"
    | Unary_op _ -> failwith "Unimplemented"
    | Binary_op _ -> failwith "Unimplemented"
    | Round_robin_pick_list l ->
      Misc_utils.get_ok_error_list (List.map (aux search_param) l)
      |> Result.map
        (Time.Intervals.Round_robin
         .merge_multi_list_round_robin_non_decreasing ~skip_check:true)
    | Round_robin_pick_seq s ->
      Seq_utils.get_ok_error_list (Seq.map (aux search_param) s)
      |> Result.map
        Time.Intervals.Round_robin
        .merge_multi_list_round_robin_non_decreasing
    | Merge_list l ->
      Misc_utils.get_ok_error_list (List.map (aux search_param) l)
      |> Result.map (Time.Intervals.Merge.merge_multi_list ~skip_check:true)
    | Merge_seq s ->
      Seq_utils.get_ok_error_list (Seq.map (aux search_param) s)
      |> Result.map (Time.Intervals.Merge.merge_multi_list ~skip_check:true)
  in
  aux search_param time

module Search_in_intervals = struct
  let resolve ?search_using_tz_offset_s (intervals : Time.Interval.t list)
      (time : Time.t) : ((int64 * int64) Seq.t, string) result =
    let search_param =
      Search_param.{ search_using_tz_offset_s; typ = Intervals intervals }
    in
    match Search_param.Check.check_search_param search_param with
    | Ok () -> resolve search_param time
    | Error _ -> Error "Invalid search intervals"
end

module Search_years_ahead = struct
  let resolve ?search_using_tz_offset_s ?(start : int64 option) years_ahead
      (time : Time.t) : ((int64 * int64) Seq.t, string) result =
    let date_time =
      match start with
      | Some x ->
        Time.Date_time.of_timestamp
          ~tz_offset_s_of_date_time:search_using_tz_offset_s x
      | None ->
        Time.Current.cur_date_time
          ~tz_offset_s_of_date_time:search_using_tz_offset_s
    in
    match date_time with
    | Ok date_time -> (
        let search_param =
          let open Search_param in
          {
            search_using_tz_offset_s;
            typ = Years_ahead { start = date_time; years_ahead };
          }
        in
        match Search_param.Check.check_search_param search_param with
        | Ok () -> resolve search_param time
        | Error _ -> Error "Invalid search years ahead or invalid start" )
    | Error () -> Error "Invalid timestamp"
end
