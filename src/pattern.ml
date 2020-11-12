type time_pattern = {
  years : int list;
  months : Time.month list;
  month_days : int list;
  weekdays : Time.weekday list;
  hours : int list;
  minutes : int list;
  seconds : int list;
  unix_seconds : int64 list;
}

type time_pattern_error =
  | Invalid_years of int list
  | Invalid_month_days of int list
  | Invalid_hours of int list
  | Invalid_minutes of int list
  | Invalid_seconds of int list
  | Invalid_unix_seconds of int64 list

type error =
  | Invalid_search_param of Search_param.error
  | Invalid_time_pattern of time_pattern_error

type time_range_pattern = time_pattern Range.range

type single_or_ranges =
  | Single_time_pattern of time_pattern
  | Time_range_patterns of time_range_pattern list

module Check = struct
  let check_time_pattern (x : time_pattern) : (unit, time_pattern_error) result
    =
    let invalid_years = List.filter (fun x -> x < 0 || 9999 < x) x.years in
    let invalid_month_days =
      List.filter (fun x -> x < 1 || 31 < x) x.month_days
    in
    let invalid_hours = List.filter (fun x -> x < 0 || 23 < x) x.hours in
    let invalid_minutes = List.filter (fun x -> x < 0 || 59 < x) x.minutes in
    let invalid_seconds = List.filter (fun x -> x < 0 || 59 < x) x.seconds in
    let invalid_unix_seconds =
      List.filter
        (fun x ->
           Result.is_error
             (Time.Date_time.of_unix_second ~tz_offset_s_of_date_time:None x))
        x.unix_seconds
    in
    match invalid_years with
    | [] -> (
        match invalid_month_days with
        | [] -> (
            match invalid_hours with
            | [] -> (
                match invalid_minutes with
                | [] -> (
                    match invalid_seconds with
                    | [] -> (
                        match invalid_unix_seconds with
                        | [] -> Ok ()
                        | l -> Error (Invalid_unix_seconds l) )
                    | l -> Error (Invalid_seconds l) )
                | l -> Error (Invalid_minutes l) )
            | l -> Error (Invalid_hours l) )
        | l -> Error (Invalid_month_days l) )
    | l -> Error (Invalid_years l)

  let check_time_range_pattern (x : time_range_pattern) :
    (unit, time_pattern_error) result =
    match x with
    | `Range_inc (x, y) | `Range_exc (x, y) -> (
        match check_time_pattern x with
        | Error e -> Error e
        | Ok () -> (
            match check_time_pattern y with
            | Error e -> Error e
            | Ok () -> Ok () ) )

  let check_search_param_and_time_pattern (search_param : Search_param.t)
      (x : time_pattern) : (unit, error) result =
    match Search_param.Check.check_search_param search_param with
    | Error e -> Error (Invalid_search_param e)
    | Ok () -> (
        match check_time_pattern x with
        | Error e -> Error (Invalid_time_pattern e)
        | Ok () -> Ok () )

  let check_search_param_and_time_range_pattern (search_param : Search_param.t)
      (x : time_range_pattern) : (unit, error) result =
    match Search_param.Check.check_search_param search_param with
    | Error e -> Error (Invalid_search_param e)
    | Ok () -> (
        match check_time_range_pattern x with
        | Error e -> Error (Invalid_time_pattern e)
        | Ok () -> Ok () )
end

let empty =
  {
    years = [];
    months = [];
    weekdays = [];
    month_days = [];
    hours = [];
    minutes = [];
    seconds = [];
    unix_seconds = [];
  }

let of_unix_second ~(tz_offset_s_of_time_pattern : Time.tz_offset_s option)
    (x : int64) : (time_pattern, unit) result =
  Time.Date_time.of_unix_second
    ~tz_offset_s_of_date_time:tz_offset_s_of_time_pattern x
  |> Result.map (fun x ->
      let open Time.Date_time in
      {
        years = [ x.year ];
        months = [ x.month ];
        weekdays = [];
        month_days = [ x.day ];
        hours = [ x.hour ];
        minutes = [ x.minute ];
        seconds = [ x.second ];
        unix_seconds = [];
      })

(* let search_in_time_zone_of_search_param (param : search_param) : Time.time_zone
   =
   match param with
   | Time_slots { search_in_time_zone; _ } -> search_in_time_zone
   | Years_ahead_start_unix_second { search_in_time_zone; _ } ->
    search_in_time_zone
   | Years_ahead_start_dt { search_in_time_zone; _ } -> search_in_time_zone
*)

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

  let matching_seconds (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Seq.t =
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

  let matching_second_ranges (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Range.range Seq.t =
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
           (cur_branch_search_start, Time.Date_time.set_to_last_sec cur_branch))
    | l ->
      List.sort_uniq compare l
      |> Time.Second_ranges.Of_list.range_seq_of_list
      |> Seq.map
        (Range.map
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

  let matching_minutes (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Seq.t =
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

  let matching_minute_ranges (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Range.range Seq.t =
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
        (Range.map
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

  let matching_hours (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Seq.t =
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

  let matching_hour_ranges (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Range.range Seq.t =
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
        (Range.map
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

  let int_month_days_of_matching_weekdays (t : time_pattern)
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

  let direct_matching_int_month_days (t : time_pattern)
      ~(cur_branch_search_start : Time.Date_time.t) : int Seq.t =
    let day_count =
      Time.day_count_of_month ~year:cur_branch_search_start.year
        ~month:cur_branch_search_start.month
    in
    match t.month_days with
    | [] -> OSeq.(cur_branch_search_start.day -- day_count)
    | l ->
      List.filter
        (fun mday -> cur_branch_search_start.day <= mday && mday <= day_count)
        l
      |> List.sort_uniq compare
      |> List.to_seq

  let matching_int_month_days (t : time_pattern)
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

  let matching_days (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Seq.t =
    let cur_branch_search_start =
      get_cur_branch_search_start ~overall_search_start cur_branch
    in
    matching_int_month_days t ~cur_branch_search_start
    |> Seq.map (fun day -> { cur_branch_search_start with day })

  let matching_day_ranges (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Range.range Seq.t =
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
      |> Seq.map (Range.map ~f_inc ~f_exc)
    | _month_days, [] ->
      direct_matching_int_month_days t ~cur_branch_search_start
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map (Range.map ~f_inc ~f_exc)
    | _, _ ->
      matching_int_month_days t ~cur_branch_search_start
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_months = struct
  let get_cur_branch_search_start ~(overall_search_start : Time.Date_time.t)
      (cur_branch : Time.Date_time.t) : Time.Date_time.t =
    if cur_branch.year = overall_search_start.year then overall_search_start
    else Time.Date_time.set_to_first_month_day_hour_min_sec cur_branch

  let matching_months (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Seq.t =
    let cur_branch_search_start =
      get_cur_branch_search_start ~overall_search_start cur_branch
    in
    let start_month_int =
      Time.human_int_of_month cur_branch_search_start.month
    in
    match t.months with
    | [] ->
      OSeq.(start_month_int -- 12)
      |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
      |> Seq.map (fun month -> { cur_branch_search_start with month })
    | pat_mon_list ->
      pat_mon_list
      |> List.to_seq
      |> Seq.map Time.human_int_of_month
      |> Seq.filter (fun month -> start_month_int <= month && month <= 12)
      |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
      |> Seq.map (fun month -> { cur_branch_search_start with month })

  let matching_month_ranges (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) (cur_branch : Time.Date_time.t)
    : Time.Date_time.t Range.range Seq.t =
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
      |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
      |> Time.Month_ranges.Of_seq.range_seq_of_seq
      |> Seq.map
        (Range.map
           ~f_inc:(range_map_inc ~cur_branch_search_start)
           ~f_exc:(range_map_exc ~cur_branch_search_start))
end

module Matching_years = struct
  let matching_years ~search_years_ahead (t : time_pattern)
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

  let matching_year_ranges ~search_years_ahead (t : time_pattern)
      ~(overall_search_start : Time.Date_time.t) :
    Time.Date_time.t Range.range Seq.t =
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
        (Range.map
           ~f_inc:(range_map_inc ~overall_search_start)
           ~f_exc:(range_map_exc ~overall_search_start))
end

module Matching_unix_seconds = struct
  let matching_unix_seconds
      ~(search_using_tz_offset_s : Time.tz_offset_s option) (t : time_pattern)
      (start : Time.Date_time.t) : Time.Date_time_set.t =
    match Time.Date_time.to_unix_second start with
    | Error () -> Time.Date_time_set.empty
    | Ok start ->
      t.unix_seconds
      |> List.sort_uniq compare
      |> List.to_seq
      |> OSeq.filter (fun x -> x >= start)
      |> Seq.filter_map (fun x ->
          match
            Time.Date_time.of_unix_second
              ~tz_offset_s_of_date_time:search_using_tz_offset_s x
          with
          | Ok x -> Some x
          | Error () -> None)
      |> Time.Date_time_set.of_seq
end

let override_search_param_possibly ~allow_search_param_override
    (search_param : Search_param.t) (t : time_pattern) : Search_param.t =
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
          { min with year = start_year; tz_offset_s = search_using_tz_offset_s }
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
                  start = `Date_time start_date_time;
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
                  start = `Date_time start_date_time;
                  years_ahead = end_inc_year - start_date_time.year + 1;
                };
          } )
  else search_param

module Single_pattern = struct
  let filter_using_matching_unix_seconds ~search_using_tz_offset_s
      (t : time_pattern) ~overall_search_start (s : Time.Date_time.t Seq.t) :
    Time.Date_time.t Seq.t =
    let matching_unix_seconds =
      Matching_unix_seconds.matching_unix_seconds ~search_using_tz_offset_s t
        overall_search_start
    in
    if Time.Date_time_set.is_empty matching_unix_seconds then s
    else Seq.filter (fun x -> Time.Date_time_set.mem x matching_unix_seconds) s

  let date_time_range_seq_of_unix_seconds ~search_using_tz_offset_s
      (s : int64 Seq.t) : Time.Date_time.t Range.range Seq.t =
    let f (x, y) =
      ( Time.Date_time.of_unix_second
          ~tz_offset_s_of_date_time:search_using_tz_offset_s x,
        Time.Date_time.of_unix_second
          ~tz_offset_s_of_date_time:search_using_tz_offset_s y )
    in
    s
    |> Ranges.Of_seq.range_seq_of_seq ~modulo:None
      ~to_int64:(fun x -> x)
      ~of_int64:(fun x -> x)
    |> Seq.map (Range.map ~f_inc:f ~f_exc:f)
    |> Seq.filter_map Range_utils.result_range_get

  let matching_date_times ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : time_pattern) :
    (Time.Date_time.t Seq.t, error) result =
    Check.check_search_param_and_time_pattern search_param t
    |> Result.map (fun () ->
        let search_param =
          override_search_param_possibly ~allow_search_param_override
            search_param t
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
          |> filter_using_matching_unix_seconds ~search_using_tz_offset_s t
            ~overall_search_start)

  let matching_unix_seconds ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : time_pattern) :
    (int64 Seq.t, error) result =
    matching_date_times ~allow_search_param_override search_param t
    |> Result.map (fun s ->
        Seq.filter_map
          (fun x ->
             match Time.Date_time.to_unix_second x with
             | Ok x -> Some x
             | Error () -> None)
          s)

  let matching_date_time_ranges ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : time_pattern) :
    (Time.Date_time.t Range.range Seq.t, error) result =
    match Check.check_search_param_and_time_pattern search_param t with
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
                t.unix_seconds )
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
            | [], [], [], [], [], [], [], unix_seconds ->
              unix_seconds
              |> List.to_seq
              |> date_time_range_seq_of_unix_seconds ~search_using_tz_offset_s
              |> Result.ok
            | ( _years,
                _months,
                _month_days,
                _weekdays,
                _hours,
                _minutes,
                _seconds,
                _unix_seconds ) ->
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
              |> filter_using_matching_unix_seconds ~search_using_tz_offset_s
                t ~overall_search_start
              |> Seq.map (fun x -> `Range_inc (x, x))
              |> Result.ok ) )

  let matching_time_slots ~allow_search_param_override
      (search_param : Search_param.t) (t : time_pattern) :
    (Time_slot.t Seq.t, error) result =
    let f (x, y) =
      (Time.Date_time.to_unix_second x, Time.Date_time.to_unix_second y)
    in
    matching_date_time_ranges ~allow_search_param_override search_param t
    |> Result.map (fun s ->
        s
        |> Seq.map (Range.map ~f_inc:f ~f_exc:f)
        |> Seq.filter_map Range_utils.result_range_get
        |> Seq.map (fun r ->
            match r with
            | `Range_inc (x, y) -> (x, Int64.succ y)
            | `Range_exc (x, y) -> (x, y))
        |> fun l ->
        let time_slots =
          match search_param.typ with
          | Time_slots time_slots ->
            let time_slots =
              time_slots |> Time_slots.Normalize.normalize_list_in_seq_out
            in
            Some time_slots
          | _ -> None
        in
        match time_slots with
        | None -> l
        | Some time_slots ->
          Time_slots.inter time_slots ~skip_check:true l
          |> Time_slots.Normalize.normalize ~skip_filter_invalid:true
            ~skip_sort:true)

  let matching_time_slots_round_robin_non_decreasing
      ~allow_search_param_override (search_param : Search_param.t)
      (l : time_pattern list) : (Time_slot.t list Seq.t, error) result =
    let l =
      List.map (matching_time_slots ~allow_search_param_override search_param) l
    in
    match List.find_opt Result.is_error l with
    | Some e -> Error (Result.get_error e)
    | None ->
      l
      |> List.map Result.get_ok
      |> Time_slots.Round_robin.collect_round_robin_non_decreasing
        ~skip_check:true
      |> OSeq.take_while (List.for_all Option.is_some)
      |> Seq.map (List.map Option.get)
      |> Result.ok

  let matching_time_slots_round_robin_non_decreasing_flat
      ~allow_search_param_override (search_param : Search_param.t)
      (l : time_pattern list) : (Time_slot.t Seq.t, error) result =
    matching_time_slots_round_robin_non_decreasing ~allow_search_param_override
      search_param l
    |> Result.map (Seq.flat_map List.to_seq)

  let next_match_date_time ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : time_pattern) :
    (Time.Date_time.t option, error) result =
    matching_date_times ~allow_search_param_override search_param t
    |> Result.map (fun s ->
        match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x)

  let next_match_unix_second ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : time_pattern) :
    (int64 option, error) result =
    next_match_date_time ~allow_search_param_override search_param t
    |> Result.map (fun x ->
        match x with
        | None -> None
        | Some x -> (
            match Time.Date_time.to_unix_second x with
            | Error () -> None
            | Ok x -> Some x ))

  let next_match_time_slot ~(allow_search_param_override : bool)
      (search_param : Search_param.t) (t : time_pattern) :
    (Time_slot.t option, error) result =
    matching_time_slots ~allow_search_param_override search_param t
    |> Result.map (fun s ->
        match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x)
end

module Range_pattern = struct
  let matching_time_slots ~allow_search_param_override
      (search_param : Search_param.t) (range : time_range_pattern) :
    (Time_slot.t Seq.t, error) result =
    let start_pat =
      match range with `Range_inc (t1, _) | `Range_exc (t1, _) -> t1
    in
    let search_param =
      override_search_param_possibly ~allow_search_param_override search_param
        start_pat
    in
    let search_and_get_start (search_param : Search_param.t) (t : time_pattern)
        ((start, _) : Time_slot.t) : Time_slot.t option =
      let search_param =
        Search_param.push_search_param_to_later_start ~start search_param
        |> Result.get_ok
      in
      match
        Single_pattern.next_match_time_slot ~allow_search_param_override:false
          search_param t
        |> Result.get_ok
      with
      | None -> None
      | Some (start', _) -> Some (start, start')
    in
    let search_and_get_end_exc (search_param : Search_param.t)
        (t : time_pattern) ((start, _) : Time_slot.t) : Time_slot.t option =
      let search_param =
        Search_param.push_search_param_to_later_start ~start search_param
        |> Result.get_ok
      in
      match
        Single_pattern.next_match_time_slot ~allow_search_param_override:false
          search_param t
        |> Result.get_ok
      with
      | None -> None
      | Some (_, end_exc') -> Some (start, end_exc')
    in
    Check.check_search_param_and_time_range_pattern search_param range
    |> Result.map (fun () ->
        let s =
          Single_pattern.matching_time_slots
            ~allow_search_param_override:false search_param start_pat
          |> Result.get_ok
        in
        match range with
        | `Range_inc (_, t2) ->
          Seq.filter_map (search_and_get_end_exc search_param t2) s
        | `Range_exc (_, t2) ->
          Seq.filter_map (search_and_get_start search_param t2) s)

  let next_match_time_slot ~allow_search_param_override
      (search_param : Search_param.t) (range : time_range_pattern) :
    ((int64 * int64) option, error) result =
    matching_time_slots ~allow_search_param_override search_param range
    |> Result.map (fun s ->
        match s () with
        | Seq.Nil -> None
        | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc))

  let matching_time_slots_multi ~allow_search_param_override
      (search_param : Search_param.t) (l : time_range_pattern list) :
    (Time_slot.t Seq.t, error) result =
    let l =
      List.map (matching_time_slots ~allow_search_param_override search_param) l
    in
    match List.find_opt Result.is_error l with
    | Some e -> Error (Result.get_error e)
    | None ->
      l
      |> List.map Result.get_ok
      |> Time_slots.Merge.merge_multi_list ~skip_check:true
      |> Result.ok

  let next_match_time_slot_multi ~allow_search_param_override
      (search_param : Search_param.t) (l : time_range_pattern list) :
    ((int64 * int64) option, error) result =
    matching_time_slots_multi ~allow_search_param_override search_param l
    |> Result.map (fun s ->
        match s () with
        | Seq.Nil -> None
        | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc))

  let matching_time_slots_round_robin_non_decreasing
      ~allow_search_param_override (search_param : Search_param.t)
      (l : time_range_pattern list) : (Time_slot.t list Seq.t, error) result =
    let l =
      List.map (matching_time_slots ~allow_search_param_override search_param) l
    in
    match List.find_opt Result.is_error l with
    | Some e -> Error (Result.get_error e)
    | None ->
      l
      |> List.map Result.get_ok
      |> Time_slots.Round_robin.collect_round_robin_non_decreasing
        ~skip_check:true
      |> OSeq.take_while (List.for_all Option.is_some)
      |> Seq.map (List.map Option.get)
      |> Result.ok

  let matching_time_slots_round_robin_non_decreasing_flat
      ~allow_search_param_override (search_param : Search_param.t)
      (l : time_range_pattern list) : (Time_slot.t Seq.t, error) result =
    matching_time_slots_round_robin_non_decreasing ~allow_search_param_override
      search_param l
    |> Result.map (Seq.flat_map List.to_seq)
end

(* module Single_or_ranges = struct
 *   let matching_time_slots (search_param : Search_param.t) (x : single_or_ranges)
 *     : (Time_slot.t Seq.t, error) result =
 *     match x with
 *     | Single_time_pattern pat ->
 *       Single_pattern.matching_time_slots search_param pat
 *     | Time_range_patterns l ->
 *       Range_pattern.matching_time_slots_multi search_param l
 * 
 *   let next_match_time_slot (search_param : Search_param.t)
 *       (x : single_or_ranges) : (Time_slot.t option, error) result =
 *     matching_time_slots search_param x
 *     |> Result.map (fun s ->
 *         match s () with
 *         | Seq.Nil -> None
 *         | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc))
 * 
 *   let matching_time_slots_round_robin_non_decreasing
 *       (search_param : Search_param.t) (t : single_or_ranges) :
 *     (Time_slot.t list Seq.t, error) result =
 *     match t with
 *     | Single_time_pattern pat ->
 *       Single_pattern.matching_time_slots_round_robin_non_decreasing
 *         search_param [ pat ]
 *     | Time_range_patterns l ->
 *       Range_pattern.matching_time_slots_round_robin_non_decreasing
 *         search_param l
 * 
 *   let matching_time_slots_round_robin_non_decreasing_flat
 *       (search_param : Search_param.t) (t : single_or_ranges) :
 *     (Time_slot.t Seq.t, error) result =
 *     matching_time_slots_round_robin_non_decreasing search_param t
 *     |> Result.map (Seq.flat_map List.to_seq)
 * end *)

module Serialize = struct
  let pack_pattern (t : time_pattern) : Time_pattern_t.time_pattern =
    {
      years = t.years;
      months = t.months;
      weekdays = t.weekdays;
      month_days = t.month_days;
      hours = t.hours;
      minutes = t.minutes;
      seconds = t.seconds;
      unix_seconds = List.map Misc_utils.int32_int32_of_int64 t.unix_seconds;
    }
end

module Deserialize = struct
  let unpack_pattern (t : Time_pattern_t.time_pattern) : time_pattern =
    {
      years = t.years;
      months = t.months;
      weekdays = t.weekdays;
      month_days = t.month_days;
      hours = t.hours;
      minutes = t.minutes;
      seconds = t.seconds;
      unix_seconds = List.map Misc_utils.int64_of_int32_int32 t.unix_seconds;
    }
end

module Equal = struct
  let equal (pat1 : time_pattern) (pat2 : time_pattern) : bool =
    List.sort compare pat1.years = List.sort compare pat2.years
    && List.sort compare pat1.months = List.sort compare pat2.months
    && List.sort compare pat1.weekdays = List.sort compare pat2.weekdays
    && List.sort compare pat1.month_days = List.sort compare pat2.month_days
    && List.sort compare pat1.hours = List.sort compare pat2.hours
    && List.sort compare pat1.minutes = List.sort compare pat2.minutes
end

module Parsers = struct
  open MParser
  open Parser_components

  let end_markers = " ]"

  let non_end_markers =
    many_satisfy (fun c -> not (String.contains end_markers c))

  let range_inc_expr (p : ('a, unit) t) : ('a Range.range, unit) t =
    attempt (p >>= fun x -> hyphen >> p >>= fun y -> return (`Range_inc (x, y)))
    <|> (p >>= fun x -> return (`Range_inc (x, x)))

  let ranges_expr ~allow_empty ~(f_flatten : 'a Range.range list -> 'a list)
      (p : ('a, unit) t) : ('a list, unit) t =
    get_pos
    >>= fun pos ->
    ( if allow_empty then sep_by_comma (range_inc_expr p)
      else sep_by_comma1 (range_inc_expr p) )
    >>= fun l ->
    try return (f_flatten l)
    with Range.Range_is_invalid ->
      fail (Printf.sprintf "Invalid range, pos: %s" (string_of_pos pos))

  let time_pattern_ranges_expr (p : ('a list, unit) t) : ('a list, unit) t =
    attempt (char '[')
    >> p
    >>= (fun l ->
        attempt (char ']' >> return l)
        <|> ( get_pos
              >>= fun pos ->
              non_square_bracket_string
              >>= fun s ->
              if s = "" then
                fail (Printf.sprintf "Missing ], pos: %s" (string_of_pos pos))
              else
                fail
                  (Printf.sprintf "Invalid ranges: %s, pos: %s" s
                     (string_of_pos pos)) ))
        <|> return []

  module Second = struct
    let second_expr =
      nat_zero
      >>= (fun x ->
          if x >= 60 then fail (Printf.sprintf "Invalid second: %d" x)
          else return x)
          <|> ( get_pos
                >>= fun pos ->
                non_end_markers
                >>= fun s ->
                fail
                  (Printf.sprintf "Invalid second term: %s, pos: %s" s
                     (string_of_pos pos)) )

    let seconds_expr ~allow_empty =
      ranges_expr ~allow_empty
        ~f_flatten:Time.Second_ranges.Flatten.flatten_list second_expr

    let seconds_cron_expr =
      attempt (char '*' >> return []) <|> seconds_expr ~allow_empty:false

    let seconds_time_pattern_expr =
      time_pattern_ranges_expr (seconds_expr ~allow_empty:true)
  end

  module Minute = struct
    let minute_expr =
      attempt nat_zero
      >>= (fun x ->
          if x >= 60 then fail (Printf.sprintf "Invalid minute: %d" x)
          else return x)
          <|> ( get_pos
                >>= fun pos ->
                non_end_markers
                >>= fun s ->
                fail
                  (Printf.sprintf "Invalid minute term: %s, pos: %s" s
                     (string_of_pos pos)) )

    let minutes_expr ~allow_empty =
      ranges_expr ~allow_empty
        ~f_flatten:Time.Minute_ranges.Flatten.flatten_list minute_expr

    let minutes_cron_expr =
      attempt (char '*' >> return []) <|> minutes_expr ~allow_empty:false

    let minutes_time_pattern_expr =
      time_pattern_ranges_expr (minutes_expr ~allow_empty:true)
  end

  module Hour = struct
    let hour_expr =
      nat_zero
      >>= (fun x ->
          if x >= 24 then fail (Printf.sprintf "Invalid hour: %d" x)
          else return x)
          <|> ( get_pos
                >>= fun pos ->
                non_end_markers
                >>= fun s ->
                fail
                  (Printf.sprintf "Invalid hour term: %s, pos: %s" s
                     (string_of_pos pos)) )

    let hours_expr ~allow_empty =
      ranges_expr ~allow_empty ~f_flatten:Time.Hour_ranges.Flatten.flatten_list
        hour_expr

    let hours_cron_expr =
      attempt (char '*' >> return []) <|> hours_expr ~allow_empty:false

    let hours_time_pattern_expr =
      time_pattern_ranges_expr (hours_expr ~allow_empty:true)
  end

  module Month_day = struct
    let month_day_expr =
      nat_zero
      >>= fun x ->
      if 1 <= x && x <= 31 then return x
      else fail (Printf.sprintf "Invalid month day: %d" x)

    let month_days_expr ~allow_empty =
      ranges_expr ~allow_empty
        ~f_flatten:Time.Month_day_ranges.Flatten.flatten_list month_day_expr

    let month_days_cron_expr =
      attempt (char '*' >> return []) <|> month_days_expr ~allow_empty:false

    let month_days_time_pattern_expr =
      time_pattern_ranges_expr (month_days_expr ~allow_empty:true)
  end

  module Month = struct
    let month_int_expr =
      nat_zero
      >>= fun x ->
      match Time.month_of_human_int x with
      | Ok x -> return x
      | Error () -> fail (Printf.sprintf "Invalid month int: %d" x)

    let month_word_expr ~for_cron =
      alpha_string
      >>= fun x ->
      if for_cron && String.length x <> 3 then
        fail (Printf.sprintf "Invalid length for month string: %s" x)
      else
        match Time.Of_string.month_of_string x with
        | Ok x -> return x
        | Error () -> fail (Printf.sprintf "Invalid month string: %s" x)

    let month_expr ~for_cron =
      (* if for_cron then ((month_word_expr ~for_cron)) <|> month_int_expr
       * else month_word_expr ~for_cron *)
      month_word_expr ~for_cron <|> month_int_expr

    let months_expr ~allow_empty ~for_cron =
      ranges_expr ~allow_empty ~f_flatten:Time.Month_ranges.Flatten.flatten_list
        (month_expr ~for_cron)

    let months_cron_expr =
      attempt (char '*' >> return [])
      <|> months_expr ~allow_empty:false ~for_cron:true

    let months_time_pattern_expr =
      time_pattern_ranges_expr (months_expr ~allow_empty:true ~for_cron:false)
  end

  module Year = struct
    let year_expr =
      attempt nat_zero
      <|> ( get_pos
            >>= fun pos ->
            non_end_markers
            >>= fun s ->
            fail
              (Printf.sprintf "Invalid year term: %s, pos: %s" s
                 (string_of_pos pos)) )

    let years_expr ~allow_empty =
      ranges_expr ~allow_empty ~f_flatten:Time.Year_ranges.Flatten.flatten_list
        year_expr

    let years_time_pattern_expr =
      time_pattern_ranges_expr (years_expr ~allow_empty:true)
  end

  module Weekday = struct
    let weekday_int_expr =
      get_pos
      >>= fun pos ->
      nat_zero
      >>= fun x ->
      match Time.weekday_of_tm_int x with
      | Ok x -> return x
      | Error () ->
        fail
          (Printf.sprintf "Invalid weekday int: %d, pos: %s" x
             (string_of_pos pos))

    let weekday_word_expr ~for_cron =
      get_pos
      >>= fun pos ->
      alpha_string
      >>= fun x ->
      if for_cron && String.length x <> 3 then
        fail
          (Printf.sprintf "Invalid length for weekday string: %s, pos: %s" x
             (string_of_pos pos))
      else
        match Time.Of_string.weekday_of_string x with
        | Ok x -> return x
        | Error () ->
          fail
            (Printf.sprintf "Invalid weekday string: %s, pos: %s" x
               (string_of_pos pos))

    let weekday_expr ~for_cron =
      if for_cron then weekday_int_expr <|> weekday_word_expr ~for_cron
      else weekday_word_expr ~for_cron

    let weekdays_expr ~allow_empty ~for_cron =
      ranges_expr ~allow_empty
        ~f_flatten:Time.Weekday_ranges.Flatten.flatten_list
        (weekday_expr ~for_cron)

    let weekdays_cron_expr =
      attempt (char '*' >> return [])
      <|> weekdays_expr ~allow_empty:false ~for_cron:true

    let weekdays_time_pattern_expr =
      time_pattern_ranges_expr (weekdays_expr ~allow_empty:true ~for_cron:false)
  end

  let cron_expr =
    Minute.minutes_cron_expr
    >>= fun minutes ->
    spaces1
    >> Hour.hours_cron_expr
    >>= fun hours ->
    spaces1
    >> Month_day.month_days_cron_expr
    >>= fun month_days ->
    spaces1
    >> Month.months_cron_expr
    >>= fun months ->
    spaces1
    >> Weekday.weekdays_cron_expr
    >>= fun weekdays ->
    eof
    >> return
      {
        years = [];
        months;
        month_days;
        weekdays;
        hours;
        minutes;
        seconds = [];
        unix_seconds = [];
      }

  let unit_char c : (unit, unit) t =
    get_pos
    >>= fun pos ->
    attempt (char c)
    >> return ()
       <|> ( satisfy (fun _ -> true)
             >>= fun c ->
             fail
               (Printf.sprintf "Invalid unit char: %c, pos: %s" c
                  (string_of_pos pos)) )

  let optional_part p = option [] (attempt p)

  let time_pattern_core_expr =
    optional_part (unit_char 'y' >> spaces >> Year.years_time_pattern_expr)
    >>= fun years ->
    spaces
    >> optional_part (unit_char 'm' >> spaces >> Month.months_time_pattern_expr)
    >>= fun months ->
    spaces
    >> optional_part
      (unit_char 'd' >> spaces >> Month_day.month_days_time_pattern_expr)
    >>= fun month_days ->
    spaces
    >> optional_part
      (unit_char 'w' >> spaces >> Weekday.weekdays_time_pattern_expr)
    >>= fun weekdays ->
    spaces
    >> optional_part (unit_char 'h' >> spaces >> Hour.hours_time_pattern_expr)
    >>= fun hours ->
    spaces
    >> unit_char 'm'
    >> spaces
    >> Minute.minutes_time_pattern_expr
    >>= fun minutes ->
    spaces
    >> optional_part
      (unit_char 's' >> spaces >> Second.seconds_time_pattern_expr)
    >>= fun seconds ->
    return
      {
        years;
        months;
        month_days;
        weekdays;
        hours;
        minutes;
        seconds;
        unix_seconds = [];
      }

  let time_pattern_expr = attempt time_pattern_core_expr <|> cron_expr
end

module Of_string = struct
  let time_pattern_of_cron_string (s : string) : (time_pattern, string) result =
    let open MParser in
    match parse_string (Parsers.cron_expr << eof) s () with
    | Success x -> Ok x
    | Failed (s, _) -> Error s

  let time_pattern_of_string (s : string) : (time_pattern, string) result =
    let open MParser in
    match parse_string (Parsers.time_pattern_expr << spaces << eof) s () with
    | Success x -> Ok x
    | Failed (s, _) -> Error s
end

module To_string = struct
  let string_of_error (e : error) : string =
    match e with
    | Invalid_search_param _ -> "Invalid search param"
    | Invalid_time_pattern _ -> "Invalid time pattern"

  let debug_string_of_weekdays (days : Time.weekday list) : string =
    let aux l =
      String.concat ","
        (List.map Time.To_string.abbreviated_string_of_weekday l)
    in
    Printf.sprintf "weekday [%s]" (aux days)

  let debug_string_of_month_days (days : int list) : string =
    let aux l = String.concat "," (List.map string_of_int l) in
    Printf.sprintf "month day [%s]" (aux days)

  let debug_string_of_time_pattern ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : time_pattern) : string =
    let aux l = String.concat "," (List.map string_of_int l) in
    let aux_months l =
      String.concat "," (List.map Time.To_string.abbreviated_string_of_month l)
    in
    Debug_print.bprintf ~indent_level buffer "time pattern :\n";
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "years : [%s]\n"
      (aux t.years);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "months : [%s]\n" (aux_months t.months);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "month days : %s\n"
      (debug_string_of_month_days t.month_days);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "weekdays : %s\n"
      (debug_string_of_weekdays t.weekdays);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "hours : [%s]\n"
      (aux t.hours);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "minutes : [%s]\n" (aux t.minutes);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "seconds : [%s]\n" (aux t.seconds);
    Buffer.contents buffer

  let debug_string_of_time_range_pattern ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : time_range_pattern) : string =
    ( match t with
      | `Range_inc (t1, t2) ->
        Debug_print.bprintf ~indent_level buffer
          "time range pattern inclusive:\n";
        debug_string_of_time_pattern ~indent_level:(indent_level + 1) ~buffer t1
        |> ignore;
        debug_string_of_time_pattern ~indent_level:(indent_level + 1) ~buffer t2
        |> ignore
      | `Range_exc (t1, t2) ->
        Debug_print.bprintf ~indent_level buffer
          "time range pattern exclusive:\n";
        debug_string_of_time_pattern ~indent_level:(indent_level + 1) ~buffer t1
        |> ignore;
        debug_string_of_time_pattern ~indent_level:(indent_level + 1) ~buffer t2
        |> ignore );
    Buffer.contents buffer

  let debug_string_of_single_or_ranges ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : single_or_ranges) : string =
    match t with
    | Single_time_pattern t ->
      debug_string_of_time_pattern ~indent_level ~buffer t
    | Time_range_patterns l ->
      List.iter
        (fun t ->
           debug_string_of_time_range_pattern ~indent_level ~buffer t |> ignore)
        l;
      Buffer.contents buffer
end

module Print = struct
  let debug_print_time_pattern ?(indent_level = 0) t =
    print_string (To_string.debug_string_of_time_pattern ~indent_level t)

  let debug_print_time_range_pattern ?(indent_level = 0) t =
    print_string (To_string.debug_string_of_time_range_pattern ~indent_level t)

  let debug_print_single_or_ranges ?(indent_level = 0) t =
    print_string (To_string.debug_string_of_single_or_ranges ~indent_level t)
end
