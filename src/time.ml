type tz_offset_s = int

let tz_offset_s_utc = 0

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

type month =
  [ `Jan
  | `Feb
  | `Mar
  | `Apr
  | `May
  | `Jun
  | `Jul
  | `Aug
  | `Sep
  | `Oct
  | `Nov
  | `Dec
  ]

type weekday_range = weekday Range.range

type month_day_range = int Range.range

type day_range =
  | Weekday_range of weekday_range
  | Month_day_range of month_day_range

let first_mday = 1

let tm_year_offset = 1900

module Int64_multipliers = struct
  let minute_to_seconds = 60L

  let hour_to_seconds = Int64.mul 60L minute_to_seconds

  let day_to_seconds = Int64.mul 24L hour_to_seconds
end

module Float_multipliers = struct
  let minute_to_seconds = Int64.to_float Int64_multipliers.minute_to_seconds

  let hour_to_seconds = Int64.to_float Int64_multipliers.hour_to_seconds

  let day_to_seconds = Int64.to_float Int64_multipliers.day_to_seconds
end

let resolve_current_tz_offset_s (x : tz_offset_s option) : tz_offset_s =
  Option.value ~default:0 x

let next_weekday (wday : weekday) : weekday =
  match wday with
  | `Sun -> `Mon
  | `Mon -> `Tue
  | `Tue -> `Wed
  | `Wed -> `Thu
  | `Thu -> `Fri
  | `Fri -> `Sat
  | `Sat -> `Sun

let tm_int_of_weekday (wday : weekday) : int =
  match wday with
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6

let weekday_of_tm_int (x : int) : (weekday, unit) result =
  match x with
  | 0 -> Ok `Sun
  | 1 -> Ok `Mon
  | 2 -> Ok `Tue
  | 3 -> Ok `Wed
  | 4 -> Ok `Thu
  | 5 -> Ok `Fri
  | 6 -> Ok `Sat
  | _ -> Error ()

let tm_int_of_month (month : month) : int =
  match month with
  | `Jan -> 0
  | `Feb -> 1
  | `Mar -> 2
  | `Apr -> 3
  | `May -> 4
  | `Jun -> 5
  | `Jul -> 6
  | `Aug -> 7
  | `Sep -> 8
  | `Oct -> 9
  | `Nov -> 10
  | `Dec -> 11

let month_of_tm_int (x : int) : (month, unit) result =
  match x with
  | 0 -> Ok `Jan
  | 1 -> Ok `Feb
  | 2 -> Ok `Mar
  | 3 -> Ok `Apr
  | 4 -> Ok `May
  | 5 -> Ok `Jun
  | 6 -> Ok `Jul
  | 7 -> Ok `Aug
  | 8 -> Ok `Sep
  | 9 -> Ok `Oct
  | 10 -> Ok `Nov
  | 11 -> Ok `Dec
  | _ -> Error ()

let human_int_of_month (month : month) : int = tm_int_of_month month + 1

let month_of_human_int (x : int) : (month, unit) result = month_of_tm_int (x - 1)

let compare_month (m1 : month) (m2 : month) : int =
  compare (tm_int_of_month m1) (tm_int_of_month m2)

let month_lt m1 m2 = tm_int_of_month m1 < tm_int_of_month m2

let month_le m1 m2 = tm_int_of_month m1 <= tm_int_of_month m2

let month_gt m1 m2 = tm_int_of_month m1 > tm_int_of_month m2

let month_ge m1 m2 = tm_int_of_month m1 >= tm_int_of_month m2

let compare_weekday (d1 : weekday) (d2 : weekday) : int =
  compare (tm_int_of_weekday d1) (tm_int_of_weekday d2)

let weekday_lt d1 d2 = tm_int_of_weekday d1 < tm_int_of_weekday d2

let weekday_le d1 d2 = tm_int_of_weekday d1 <= tm_int_of_weekday d2

let weekday_gt d1 d2 = tm_int_of_weekday d1 > tm_int_of_weekday d2

let weekday_ge d1 d2 = tm_int_of_weekday d1 >= tm_int_of_weekday d2

let zero_tm_sec tm = Unix.{ tm with tm_sec = 0 }

let is_leap_year ~year =
  assert (year >= 0);
  let divisible_by_4 = year mod 4 = 0 in
  let divisible_by_100 = year mod 100 = 0 in
  let divisible_by_400 = year mod 400 = 0 in
  divisible_by_4 && ((not divisible_by_100) || divisible_by_400)

let day_count_of_year ~year = if is_leap_year ~year then 366 else 365

let day_count_of_month ~year ~(month : month) =
  match month with
  | `Jan -> 31
  | `Feb -> if is_leap_year ~year then 29 else 28
  | `Mar -> 31
  | `Apr -> 30
  | `May -> 31
  | `Jun -> 30
  | `Jul -> 31
  | `Aug -> 31
  | `Sep -> 30
  | `Oct -> 31
  | `Nov -> 30
  | `Dec -> 31

let weekday_of_month_day ~(year : int) ~(month : month) ~(mday : int) :
  (weekday, unit) result =
  match Ptime.(of_date (year, human_int_of_month month, mday)) with
  | None -> Error ()
  | Some wday -> Ok (Ptime.weekday wday)

module Second_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Minute_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Hour_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Weekday_tm_int_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = Some 7

    let to_int x = x

    let of_int x = x
  end)

module Weekday_ranges = Ranges_small.Make (struct
    type t = weekday

    let modulo = Some 7

    let to_int = tm_int_of_weekday

    let of_int x = x |> weekday_of_tm_int |> Result.get_ok
  end)

module Month_day_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Month_tm_int_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Month_ranges = Ranges_small.Make (struct
    type t = month

    let modulo = None

    let to_int = human_int_of_month

    let of_int x = x |> month_of_human_int |> Result.get_ok
  end)

module Year_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Date_time = struct
  type t = {
    year : int;
    month : month;
    day : int;
    hour : int;
    minute : int;
    second : int;
    tz_offset_s : int;
  }

  let to_ptime_date_time (x : t) : Ptime.date * Ptime.time =
    ( (x.year, human_int_of_month x.month, x.day),
      ((x.hour, x.minute, x.second), x.tz_offset_s) )

  let of_ptime_date_time
      (((year, month, day), ((hour, minute, second), tz_offset_s)) :
         Ptime.date * Ptime.time) : (t, unit) result =
    match month_of_human_int month with
    | Ok month -> Ok { year; month; day; hour; minute; second; tz_offset_s }
    | Error () -> Error ()

  let to_unix_second (x : t) : (int64, unit) result =
    match Ptime.of_date_time (to_ptime_date_time x) with
    | None -> Error ()
    | Some x -> x |> Ptime.to_float_s |> Int64.of_float |> Result.ok

  let of_unix_second ~(tz_offset_s_of_date_time : tz_offset_s option)
      (x : int64) : (t, unit) result =
    match Ptime.of_float_s (Int64.to_float x) with
    | None -> Error ()
    | Some x ->
      let tz_offset_s =
        resolve_current_tz_offset_s tz_offset_s_of_date_time
      in
      x |> Ptime.to_date_time ~tz_offset_s |> of_ptime_date_time

  let min =
    Ptime.min |> Ptime.to_date_time |> of_ptime_date_time |> Result.get_ok

  let max =
    Ptime.max |> Ptime.to_date_time |> of_ptime_date_time |> Result.get_ok

  let compare (x : t) (y : t) : int =
    match compare x.year y.year with
    | 0 -> (
        match
          compare (human_int_of_month x.month) (human_int_of_month y.month)
        with
        | 0 -> (
            match compare x.day y.day with
            | 0 -> (
                match compare x.hour y.hour with
                | 0 -> (
                    match compare x.minute y.minute with
                    | 0 -> compare x.second y.second
                    | n -> n )
                | n -> n )
            | n -> n )
        | n -> n )
    | n -> n

  let set_to_first_sec (x : t) : t = { x with second = 0 }

  let set_to_last_sec (x : t) : t = { x with second = 59 }

  let set_to_first_min_sec (x : t) : t =
    { x with minute = 0 } |> set_to_first_sec

  let set_to_last_min_sec (x : t) : t =
    { x with minute = 59 } |> set_to_last_sec

  let set_to_first_hour_min_sec (x : t) : t =
    { x with hour = 0 } |> set_to_first_min_sec

  let set_to_last_hour_min_sec (x : t) : t =
    { x with hour = 23 } |> set_to_last_min_sec

  let set_to_first_day_hour_min_sec (x : t) : t =
    { x with day = 1 } |> set_to_first_hour_min_sec

  let set_to_last_day_hour_min_sec (x : t) : t =
    { x with day = day_count_of_month ~year:x.year ~month:x.month }
    |> set_to_last_hour_min_sec

  let set_to_first_month_day_hour_min_sec (x : t) : t =
    { x with month = `Jan } |> set_to_first_day_hour_min_sec

  let set_to_last_month_day_hour_min_sec (x : t) : t =
    { x with month = `Dec } |> set_to_last_day_hour_min_sec
end

module Check = struct
  let unix_second_is_valid (x : int64) : bool =
    match Date_time.of_unix_second ~tz_offset_s_of_date_time:None x with
    | Ok _ -> true
    | Error () -> false

  let second_is_valid ~(second : int) : bool = 0 <= second && second < 60

  let minute_second_is_valid ~(minute : int) ~(second : int) : bool =
    0 <= minute && minute < 60 && second_is_valid ~second

  let hour_minute_second_is_valid ~(hour : int) ~(minute : int) ~(second : int)
    : bool =
    (0 <= hour && hour < 24) && minute_second_is_valid ~minute ~second

  let date_time_is_valid (x : Date_time.t) : bool =
    match Date_time.to_unix_second x with Ok _ -> true | Error () -> false
end

let next_hour_minute ~(hour : int) ~(minute : int) : (int * int, unit) result =
  if Check.hour_minute_second_is_valid ~hour ~minute ~second:0 then
    if minute < 59 then Ok (hour, succ minute) else Ok (succ hour mod 24, 0)
  else Error ()

module Pattern = struct
  type pattern = {
    years : int list;
    months : month list;
    month_days : int list;
    weekdays : weekday list;
    hours : int list;
    minutes : int list;
    seconds : int list;
    unix_seconds : int64 list;
  }

  type pattern_error =
    | Invalid_years of int list
    | Invalid_month_days of int list
    | Invalid_hours of int list
    | Invalid_minutes of int list
    | Invalid_seconds of int list
    | Invalid_unix_seconds of int64 list

  type range_pattern = pattern Range.range

  module Check = struct
    let check_pattern (x : pattern) : (unit, pattern_error) result
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
               (Date_time.of_unix_second ~tz_offset_s_of_date_time:None x))
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

    let check_range_pattern (x : range_pattern) :
      (unit, pattern_error) result =
      match x with
      | `Range_inc (x, y) | `Range_exc (x, y) -> (
          match check_pattern x with
          | Error e -> Error e
          | Ok () -> (
              match check_pattern y with
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

end
