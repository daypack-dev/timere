open Date_time_components

let day_index_of_weekday (weekday : weekday) =
  match weekday with
  | `Mon -> 0
  | `Tue -> 1
  | `Wed -> 2
  | `Thu -> 3
  | `Fri -> 4
  | `Sat -> 5
  | `Sun -> 6

module ISO_week_date = struct
  type t = {
    iso_week_year : int;
    week : int;
    weekday : weekday;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_iso_week_year of int
    | `Invalid_week of int
    ]

  exception Error_exn of error

  let equal (x : t) (y : t) : bool =
    x.iso_week_year = y.iso_week_year
    && x.week = y.week
    && x.weekday = y.weekday

  let make ~iso_week_year ~week ~weekday : (t, error) result =
    if iso_week_year < Constants.min_year || Constants.max_year < iso_week_year
    then Error (`Invalid_iso_week_year iso_week_year)
    else if week < 1 || (week_count_of_iso_week_year ~iso_week_year) < week then Error (`Invalid_week week)
    else
      Ok { iso_week_year; week; weekday }

  let make_exn ~iso_week_year ~week ~weekday : t =
    match make ~iso_week_year ~week ~weekday with
    | Error e -> raise (Error_exn e)
    | Ok x -> x
end

module Ymd_date = struct
  type t = {
    year : int;
    month : int;
    day : int;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_month of int
    | `Invalid_day of int
    ]

  exception Error_exn of error

  let equal (x : t) (y : t) : bool =
    x.year = y.year && x.month = y.month && x.day = y.day

  let make ~year ~month ~day : (t, error) result =
    if year < Constants.min_year || Constants.max_year < year then
      Error (`Invalid_year year)
    else if month < 1 || 12 < month then Error (`Invalid_month month)
    else if day < 1 || (day_count_of_month ~year ~month) < day then Error (`Invalid_day day)
    else Ok { year; month; day }

  let make_exn ~year ~month ~day : t =
    match make ~year ~month ~day with
    | Error e -> raise (Error_exn e)
    | Ok x -> x
end

module ISO_ord_date = struct
  type t = {
    year : int;
    day_of_year : int;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_day_of_year of int
    ]

  exception Error_exn of error

  let equal (x : t) (y : t) : bool =
    x.year = y.year && x.day_of_year = y.day_of_year

  let make ~year ~day_of_year : (t, error) result =
    if year < Constants.min_year || Constants.max_year < year then
      Error (`Invalid_year year)
    else if day_of_year < 1 || (day_count_of_year ~year) < day_of_year then
      Error (`Invalid_day_of_year day_of_year)
    else Ok { year; day_of_year }

  let make_exn ~year ~day_of_year : t =
    match make ~year ~day_of_year with
    | Error e -> raise (Error_exn e)
    | Ok x -> x

  let weekday ({ year; day_of_year } : t) =
    let month, day = md_of_ydoy ~year ~day_of_year in
    weekday_of_ymd ~year ~month ~day

  let to_iso_week_date (x : t) : ISO_week_date.t =
    let weekday = weekday x in
    let week_of_year =
      (10 + x.day_of_year - (day_index_of_weekday weekday + 1)) / 7
    in
    assert (week_of_year >= 0);
    assert (week_of_year <= 53);
    let iso_week_year, week =
      if week_of_year = 0 then
        if x.year = 0 then (-1, 52)
        else
          (pred x.year, week_count_of_iso_week_year ~iso_week_year:(pred x.year))
      else if
        week_of_year = 53
        && week_count_of_iso_week_year ~iso_week_year:x.year < 53
      then (succ x.year, 1)
      else (x.year, week_of_year)
    in
    { iso_week_year; week; weekday }

  let of_iso_week_date ({ iso_week_year; week; weekday } : ISO_week_date.t) : t
    =
    let weekday_int = day_index_of_weekday weekday + 1 in
    let jan_4_weekday_int =
      day_index_of_weekday (weekday_of_ymd ~year:iso_week_year ~month:1 ~day:4)
      + 1
    in
    let day_of_year = (week * 7) + weekday_int - (jan_4_weekday_int + 3) in
    let day_count_of_prev_year = day_count_of_year ~year:(pred iso_week_year) in
    let day_count_of_cur_year = day_count_of_year ~year:iso_week_year in
    let year, day_of_year =
      if day_of_year < 0 then
        (pred iso_week_year, day_count_of_prev_year - day_of_year)
      else if day_of_year > day_count_of_cur_year then
        (succ iso_week_year, day_of_year - day_count_of_cur_year)
      else (iso_week_year, day_count_of_cur_year)
    in
    { year; day_of_year }

  let to_ymd_date ({ year; day_of_year } : t) : Ymd_date.t =
    let month, day = md_of_ydoy ~year ~day_of_year in
    { year; month; day }

  let of_ymd_date ({ year; month; day } : Ymd_date.t) : t =
    let day_of_year = doy_of_ymd ~year ~month ~day in
    { year; day_of_year }
end
