open Date_time_components

type t = { jd : int }

let equal (x : t) (y : t) : bool = x.jd = y.jd

let weekday (x : t) = weekday_of_jd x.jd

module ISO_week_date = struct
  type view = {
    iso_week_year : int;
    iso_week : int;
    weekday : weekday;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_iso_week_year of int
    | `Invalid_iso_week of int
    ]

  exception Error_exn of error

  let make ~iso_week_year ~iso_week ~weekday : (t, error) result =
    if iso_week_year < Constants.min_year || Constants.max_year < iso_week_year
    then Error (`Invalid_iso_week_year iso_week_year)
    else if
      iso_week < 1 || week_count_of_iso_week_year ~iso_week_year < iso_week
    then Error (`Invalid_iso_week iso_week)
    else Ok { jd = jd_of_iso_week_date ~iso_week_year ~iso_week ~weekday }

  let make_exn ~iso_week_year ~iso_week ~weekday : t =
    match make ~iso_week_year ~iso_week ~weekday with
    | Error e -> raise (Error_exn e)
    | Ok x -> x

  let view (x : t) : view =
    let iso_week_year, iso_week, weekday = iso_week_date_of_jd x.jd in
    { iso_week_year; iso_week; weekday }
end

module Ymd_date = struct
  type view = {
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

  let make ~year ~month ~day : (t, error) result =
    if year < Constants.min_year || Constants.max_year < year then
      Error (`Invalid_year year)
    else if month < 1 || 12 < month then Error (`Invalid_month month)
    else if day < 1 || day_count_of_month ~year ~month < day then
      Error (`Invalid_day day)
    else Ok { jd = jd_of_ymd ~year ~month ~day }

  let make_exn ~year ~month ~day : t =
    match make ~year ~month ~day with
    | Error e -> raise (Error_exn e)
    | Ok x -> x

  let view (x : t) : view =
    let year, month, day = ymd_of_jd x.jd in
    { year; month; day }
end

module ISO_ord_date = struct
  type view = {
    year : int;
    day_of_year : int;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_day_of_year of int
    ]

  exception Error_exn of error

  let make ~year ~day_of_year : (t, error) result =
    if year < Constants.min_year || Constants.max_year < year then
      Error (`Invalid_year year)
    else if day_of_year < 1 || day_count_of_year ~year < day_of_year then
      Error (`Invalid_day_of_year day_of_year)
    else Ok { jd = jd_of_ydoy ~year ~day_of_year }

  let make_exn ~year ~day_of_year : t =
    match make ~year ~day_of_year with
    | Error e -> raise (Error_exn e)
    | Ok x -> x

  let view (x : t) : view =
    let year, month, day = ymd_of_jd x.jd in
    let day_of_year = doy_of_ymd ~year ~month ~day in
    { year; day_of_year }
end

let year d = (Ymd_date.view d).year

let month d = (Ymd_date.view d).month

let day d = (Ymd_date.view d).day

let iso_week_year d = (ISO_week_date.view d).iso_week_year

let iso_week d = (ISO_week_date.view d).iso_week

let day_of_year d = (ISO_ord_date.view d).day_of_year
