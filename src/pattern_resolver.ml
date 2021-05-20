module Branch = struct
  type t = {
    year : int;
    month : int;
    day : int;
    hour : int;
    minute : int;
    second : int;
    ns : int;
  }

  let to_date_time ~offset_from_utc (x : t) : Timedesc.t option =
    match
      Timedesc.make_unambiguous ~offset_from_utc ~year:x.year ~month:x.month
        ~day:x.day ~hour:x.hour ~minute:x.minute ~second:x.second ~ns:x.ns ()
    with
    | Ok t -> Some t
    | Error `Does_not_exist -> None
    | Error _ -> failwith "Unexpected case"

  let of_date_time (x : Timedesc.t) : t =
    let { Timedesc.Date.Ymd_date.year; month; day } = Timedesc.ymd_date x in
    let { Timedesc.Time.hour; minute; second; ns } = Timedesc.time_view x in
    { year; month; day; hour; minute; second; ns }

  let set_to_first_ns (x : t) : t = { x with ns = 0 }

  let set_to_last_ns (x : t) : t =
    { x with ns = Timedesc.Span.ns_count_in_s - 1 }

  let set_to_first_sec_ns (x : t) : t = { x with second = 0 } |> set_to_first_ns

  let set_to_last_sec_ns (x : t) : t = { x with second = 59 } |> set_to_last_ns

  let set_to_first_min_sec_ns (x : t) : t =
    { x with minute = 0 } |> set_to_first_sec_ns

  let set_to_last_min_sec_ns (x : t) : t =
    { x with minute = 59 } |> set_to_last_sec_ns

  let set_to_first_hour_min_sec_ns (x : t) : t =
    { x with hour = 0 } |> set_to_first_min_sec_ns

  let set_to_last_hour_min_sec_ns (x : t) : t =
    { x with hour = 23 } |> set_to_last_min_sec_ns

  let set_to_first_day_hour_min_sec_ns (x : t) : t =
    { x with day = 1 } |> set_to_first_hour_min_sec_ns

  let set_to_last_day_hour_min_sec_ns (x : t) : t =
    {
      x with
      day = Timedesc.Utils.day_count_of_month ~year:x.year ~month:x.month;
    }
    |> set_to_last_hour_min_sec_ns

  let set_to_first_month_day_hour_min_sec_ns (x : t) : t =
    { x with month = 1 } |> set_to_first_day_hour_min_sec_ns

  let set_to_last_month_day_hour_min_sec_ns (x : t) : t =
    { x with month = 12 } |> set_to_last_day_hour_min_sec_ns
end

module Search_param = struct
  type t = {
    search_using_offset_from_utc : Timedesc.Span.t;
    start : Branch.t;
    end_inc : Branch.t;
    start_dt : Timedesc.t;
    end_inc_dt : Timedesc.t;
  }

  let make ~search_using_offset_from_utc_s ((start, end_exc) : Time.Interval'.t)
    : t =
    let search_using_offset_from_utc =
      Timedesc.Span.make_small ~s:search_using_offset_from_utc_s ()
    in
    let tz_of_date_time =
      Timedesc.Time_zone.make_offset_only_exn search_using_offset_from_utc
    in
    let start_dt =
      CCOpt.get_exn_or
        "Expected successful date time construction from timestamp"
      @@ Timedesc.of_timestamp ~tz_of_date_time start
    in
    let end_inc_dt =
      CCOpt.get_exn_or
        "Expected successful date time construction from timestamp"
      @@ Timedesc.of_timestamp ~tz_of_date_time (Timedesc.Span.pred end_exc)
    in
    {
      search_using_offset_from_utc;
      start = Branch.of_date_time start_dt;
      end_inc = Branch.of_date_time end_inc_dt;
      start_dt;
      end_inc_dt;
    }
end

let failwith_unexpected_case (_ : 'a) = failwith "Unexpected case"

module Matching_seconds = struct
  let matching_second_ranges (t : Pattern.t) (cur_branch : Branch.t) :
    Branch.t Time.Range.range Seq.t =
    let cur_branch_search_start = Branch.set_to_first_sec_ns cur_branch in
    let cur_branch_search_end_inc = Branch.set_to_last_sec_ns cur_branch in
    let range_map_inc ~(cur_branch : Branch.t) (x, y) =
      ( Branch.set_to_first_ns { cur_branch with second = x },
        Branch.set_to_last_ns { cur_branch with second = y } )
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
  let get_cur_branch_search_start (cur_branch : Branch.t) : Branch.t =
    Branch.set_to_first_min_sec_ns cur_branch

  let get_cur_branch_search_end_inc (cur_branch : Branch.t) : Branch.t =
    Branch.set_to_last_min_sec_ns cur_branch

  let matching_minutes (t : Pattern.t) (cur_branch : Branch.t) : Branch.t Seq.t
    =
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

  let matching_minute_ranges (t : Pattern.t) (cur_branch : Branch.t) :
    Branch.t Time.Range.range Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let range_map_inc ~(cur_branch : Branch.t) (x, y) =
      ( Branch.set_to_first_sec_ns { cur_branch with minute = x },
        Branch.set_to_last_sec_ns { cur_branch with minute = y } )
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
  let get_cur_branch_search_start (cur_branch : Branch.t) : Branch.t =
    Branch.set_to_first_hour_min_sec_ns cur_branch

  let get_cur_branch_search_end_inc (cur_branch : Branch.t) : Branch.t =
    Branch.set_to_last_hour_min_sec_ns cur_branch

  let matching_hours (t : Pattern.t) (cur_branch : Branch.t) : Branch.t Seq.t =
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

  let matching_hour_ranges (t : Pattern.t) (cur_branch : Branch.t) :
    Branch.t Time.Range.range Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let range_map_inc ~(cur_branch : Branch.t) (x, y) =
      ( Branch.set_to_first_min_sec_ns { cur_branch with hour = x },
        Branch.set_to_last_min_sec_ns { cur_branch with hour = y } )
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
  let get_cur_branch_search_start (cur_branch : Branch.t) : Branch.t =
    Branch.set_to_first_day_hour_min_sec_ns cur_branch

  let get_cur_branch_search_end_inc (cur_branch : Branch.t) : Branch.t =
    Branch.set_to_last_day_hour_min_sec_ns cur_branch

  let int_month_days_of_matching_weekdays ~(cur_branch : Branch.t)
      (t : Pattern.t) : int Seq.t =
    let day_count =
      Timedesc.Utils.day_count_of_month ~year:cur_branch.year
        ~month:cur_branch.month
    in
    let day_start = 1 in
    let day_end_inc = day_count in
    if Weekday_set.is_empty t.weekdays then OSeq.(day_start -- day_end_inc)
    else
      OSeq.(day_start -- day_end_inc)
      |> Seq.filter (fun day ->
          let wday =
            Timedesc.Date.Ymd_date.make_exn ~year:cur_branch.year
              ~month:cur_branch.month ~day
            |> Timedesc.Date.weekday
          in
          Weekday_set.mem wday t.weekdays)

  let direct_matching_int_month_days ~(cur_branch : Branch.t) (t : Pattern.t) :
    int Seq.t =
    let day_count =
      Timedesc.Utils.day_count_of_month ~year:cur_branch.year
        ~month:cur_branch.month
    in
    let day_start = 1 in
    let day_end_inc = day_count in
    if Int_set.is_empty t.month_days then OSeq.(day_start -- day_end_inc)
    else
      t.month_days
      |> Int_set.to_seq
      |> Seq.map (fun mday -> if mday < 0 then day_count + mday + 1 else mday)
      |> Seq.filter (fun mday -> mday <= day_end_inc)

  let matching_int_month_days ~(cur_branch : Branch.t) (t : Pattern.t) :
    int Seq.t =
    let matching_month_days =
      direct_matching_int_month_days t ~cur_branch |> Int_set.of_seq
    in
    let month_days_of_matching_weekdays =
      int_month_days_of_matching_weekdays t ~cur_branch |> Int_set.of_seq
    in
    Int_set.inter matching_month_days month_days_of_matching_weekdays
    |> Int_set.to_seq

  let matching_days (t : Pattern.t) (cur_branch : Branch.t) : Branch.t Seq.t =
    matching_int_month_days t ~cur_branch
    |> Seq.map (fun day -> { cur_branch with day })

  let matching_day_ranges (t : Pattern.t) (cur_branch : Branch.t) :
    Branch.t Time.Range.range Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let range_map_inc ~(cur_branch : Branch.t) (x, y) =
      ( Branch.set_to_first_hour_min_sec_ns { cur_branch with day = x },
        Branch.set_to_last_hour_min_sec_ns { cur_branch with day = y } )
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
  let get_cur_branch_search_start (cur_branch : Branch.t) : Branch.t =
    Branch.set_to_first_month_day_hour_min_sec_ns cur_branch

  let get_cur_branch_search_end_inc (cur_branch : Branch.t) : Branch.t =
    Branch.set_to_last_month_day_hour_min_sec_ns cur_branch

  let matching_months (t : Pattern.t) (cur_branch : Branch.t) : Branch.t Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let month_start = cur_branch_search_start.month in
    let month_end_inc = cur_branch_search_end_inc.month in
    if Int_set.is_empty t.months then
      OSeq.(month_start -- month_end_inc)
      |> Seq.map (fun month -> { cur_branch_search_start with month })
    else
      t.months
      |> Int_set.to_seq
      |> Seq.filter (fun month ->
          month_start <= month && month <= month_end_inc)
      |> Seq.map (fun month -> { cur_branch_search_start with month })

  let matching_month_ranges (t : Pattern.t) (cur_branch : Branch.t) :
    Branch.t Time.Range.range Seq.t =
    let cur_branch_search_start = get_cur_branch_search_start cur_branch in
    let cur_branch_search_end_inc = get_cur_branch_search_end_inc cur_branch in
    let range_map_inc ~(cur_branch : Branch.t) (x, y) =
      ( Branch.set_to_first_day_hour_min_sec_ns { cur_branch with month = x },
        Branch.set_to_last_day_hour_min_sec_ns { cur_branch with month = y } )
    in
    let month_start = cur_branch_search_start.month in
    let month_end_inc = cur_branch_search_end_inc.month in
    if Int_set.is_empty t.months then
      Seq.return
        (`Range_inc (cur_branch_search_start, cur_branch_search_end_inc))
    else
      t.months
      |> Int_set.to_seq
      |> Seq.filter (fun month ->
          month_start <= month && month <= month_end_inc)
      |> Time.Month_ranges.Of_seq.range_seq_of_seq
      |> Seq.map
        (Time.Range.map
           ~f_inc:(range_map_inc ~cur_branch)
           ~f_exc:failwith_unexpected_case)
end

module Matching_years = struct
  let matching_years ~(overall_search_start : Branch.t)
      ~(overall_search_end_inc : Branch.t) (t : Pattern.t) : Branch.t Seq.t =
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

  let matching_year_ranges ~(overall_search_start : Branch.t)
      ~(overall_search_end_inc : Branch.t) (t : Pattern.t) :
    Branch.t Time.Range.range Seq.t =
    let range_map_inc ~(overall_search_start : Branch.t)
        ~(overall_search_end_inc : Branch.t) (x, y) =
      let range_map_start =
        Branch.set_to_first_month_day_hour_min_sec_ns
          { overall_search_start with year = x }
      in
      let range_map_end_inc =
        Branch.set_to_last_month_day_hour_min_sec_ns
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
  Branch.t Time.Range.range Seq.t =
  let overall_search_start =
    Branch.set_to_first_month_day_hour_min_sec_ns search_param.start
  in
  let overall_search_end_inc =
    Branch.set_to_last_month_day_hour_min_sec_ns search_param.end_inc
  in
  match
    ( Int_set.is_empty t.years,
      Int_set.is_empty t.months,
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

let one_ns = Timedesc.Span.make ~ns:1 ()

let resolve (search_param : Search_param.t) (t : Pattern.t) :
  (Timedesc.Span.t * Timedesc.Span.t) Seq.t =
  let f (x, y) =
    let x =
      x
      |> Branch.to_date_time
        ~offset_from_utc:search_param.search_using_offset_from_utc
      |> CCOpt.map Timedesc.to_timestamp_single
    in
    let y =
      y
      |> Branch.to_date_time
        ~offset_from_utc:search_param.search_using_offset_from_utc
      |> CCOpt.map Timedesc.to_timestamp_single
    in
    match (x, y) with
    | Some x, Some y -> Some (x, y)
    | None, None -> None
    | None, Some y -> Some (Timedesc.Timestamp.min_val, y)
    | Some x, None -> Some (x, Timedesc.Timestamp.max_val)
  in
  matching_date_time_ranges search_param t
  |> Seq.map (fun r ->
      match r with
      | `Range_inc (x, y) -> (x, y)
      | `Range_exc _ -> failwith "Unexpected case")
  |> Seq.filter_map f
  |> Seq.map (fun (x, y) -> (x, Timestamp_utils.timestamp_safe_add y one_ns))
  |> Time.Intervals.normalize ~skip_filter_invalid:true ~skip_sort:true
  |> Time.Intervals.Slice.slice
    ~start:(Timedesc.to_timestamp_single search_param.start_dt)
    ~end_exc:
      (Timedesc.Span.succ
       @@ Timedesc.to_timestamp_single search_param.end_inc_dt)
