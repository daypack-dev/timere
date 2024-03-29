open Date_time_utils

type t = { jd : int }

let equal (x : t) (y : t) : bool = x.jd = y.jd

let lt (x : t) (y : t) = x.jd < y.jd

let le (x : t) (y : t) = x.jd <= y.jd

let gt (x : t) (y : t) = x.jd > y.jd

let ge (x : t) (y : t) = x.jd >= y.jd

let compare (x : t) (y : t) = compare x.jd y.jd

let add ~days (t : t) =
  { jd = t.jd + days }

let sub ~days (t : t) =
  { jd = t.jd - days }

let diff_days (x : t) (y : t) =
  x.jd - y.jd

let weekday (x : t) = weekday_of_jd x.jd

module ISO_week_date' = struct
  type view = {
    year : int;
    week : int;
    weekday : weekday;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_iso_year of int
    | `Invalid_iso_week of int
    ]

  exception Error_exn of error

  let of_iso_week (x : ISO_week.t) ~weekday : t =
    let year, week = ISO_week.year_week x in
    { jd = jd_of_iso_week_date ~year ~week ~weekday }

  let make ~year ~week ~weekday : (t, error) result =
    match ISO_week.make ~year ~week with
    | Error e -> Error (e :> error)
    | Ok x -> Ok (of_iso_week x ~weekday)

  let make_exn ~year ~week ~weekday : t =
    match make ~year ~week ~weekday with
    | Error e -> raise (Error_exn e)
    | Ok x -> x

  let view (x : t) : view =
    let year, week, weekday = iso_week_date_of_jd x.jd in
    { year; week; weekday }
end

let of_iso_week = ISO_week_date'.of_iso_week

module Ymd' = struct
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

  let of_ym (x : Ym.t) ~day : (t, error) result =
    let year, month = Ym.year_month x in
    if day < 1 || day_count_of_month ~year ~month < day then
      Error (`Invalid_day day)
    else Ok { jd = jd_of_ymd ~year ~month ~day }

  let of_ym_exn x ~day =
    match of_ym x ~day with Error e -> raise (Error_exn e) | Ok x -> x

  let make ~year ~month ~day : (t, error) result =
    match Ym.make ~year ~month with
    | Error e -> Error (e :> error)
    | Ok x -> of_ym x ~day

  let make_exn ~year ~month ~day : t =
    match make ~year ~month ~day with
    | Error e -> raise (Error_exn e)
    | Ok x -> x

  let view (x : t) : view =
    let year, month, day = ymd_of_jd x.jd in
    { year; month; day }
end

let of_ym = Ymd'.of_ym

let of_ym_exn = Ymd'.of_ym_exn

module ISO_ord' = struct
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

let year d = (Ymd'.view d).year

let month d = (Ymd'.view d).month

let day d = (Ymd'.view d).day

let ym d =
  let Ymd'.{ year; month; _ } = Ymd'.view d in
  Ym.make_exn ~year ~month

let iso_year d =
  let ISO_week_date'.{ year; _ } = ISO_week_date'.view d in
  year

let iso_week d =
  let ISO_week_date'.{ year; week; _ } = ISO_week_date'.view d in
  ISO_week.make_exn ~year ~week

let day_of_year d = (ISO_ord'.view d).day_of_year
