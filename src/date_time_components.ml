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

let tm_int_of_weekday (wday : weekday) : int =
  match wday with
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6

let weekday_of_tm_int (x : int) : weekday option =
  match x with
  | 0 -> Some `Sun
  | 1 -> Some `Mon
  | 2 -> Some `Tue
  | 3 -> Some `Wed
  | 4 -> Some `Thu
  | 5 -> Some `Fri
  | 6 -> Some `Sat
  | _ -> None

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

let month_of_tm_int (x : int) : month option =
  match x with
  | 0 -> Some `Jan
  | 1 -> Some `Feb
  | 2 -> Some `Mar
  | 3 -> Some `Apr
  | 4 -> Some `May
  | 5 -> Some `Jun
  | 6 -> Some `Jul
  | 7 -> Some `Aug
  | 8 -> Some `Sep
  | 9 -> Some `Oct
  | 10 -> Some `Nov
  | 11 -> Some `Dec
  | _ -> None

let human_int_of_month (month : month) : int = tm_int_of_month month + 1

let month_of_human_int (x : int) : month option = month_of_tm_int (x - 1)

let compare_month (m1 : month) (m2 : month) : int =
  compare (tm_int_of_month m1) (tm_int_of_month m2)

module Month_set = struct
  include CCSet.Make (struct
      type t = month

      let compare = compare_month
    end)

  let to_seq x = x |> to_list |> CCList.to_seq
end

let compare_weekday (d1 : weekday) (d2 : weekday) : int =
  compare (tm_int_of_weekday d1) (tm_int_of_weekday d2)

module Weekday_set = struct
  include CCSet.Make (struct
      type t = weekday

      let compare = compare_weekday
    end)

  let to_seq x = x |> to_list |> CCList.to_seq
end

let weekday_of_month_day ~(year : int) ~(month : month) ~(mday : int) :
  weekday option =
  Ptime.(of_date (year, human_int_of_month month, mday))
  |> CCOpt.map Ptime.weekday

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

type tz_info =
  [ `Tz_only of Time_zone.t
  | `Tz_offset_s_only of int
  | `Tz_and_tz_offset_s of Time_zone.t * int
  ]

let equal_tz_info (x : tz_info) (y : tz_info) =
  match (x, y) with
  | `Tz_only x, `Tz_only y -> Time_zone.equal x y
  | `Tz_offset_s_only x, `Tz_offset_s_only y -> x = y
  | `Tz_and_tz_offset_s (tz1, x1), `Tz_and_tz_offset_s (tz2, x2) ->
    Time_zone.equal tz1 tz2 && x1 = x2
  | _, _ -> false

let make_tz_info ?tz ?tz_offset_s () =
  match (tz, tz_offset_s) with
  | None, None -> invalid_arg "make_tz_info"
  | Some tz, None -> Some (`Tz_only tz)
  | None, Some tz_offset_s -> Some (`Tz_offset_s_only tz_offset_s)
  | Some tz, Some tz_offset_s ->
    if Time_zone.offset_is_recorded tz_offset_s tz then
      Some (`Tz_and_tz_offset_s (tz, tz_offset_s))
    else None

let tz_offset_s_of_tz_info (x : tz_info) =
  match x with
  | `Tz_only _ -> None
  | `Tz_offset_s_only offset | `Tz_and_tz_offset_s (_, offset) -> Some offset
