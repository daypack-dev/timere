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

let weekday_of_month_day ~(year : int) ~(month : month) ~(day : int) :
  weekday option =
  Ptime.(of_date (year, human_int_of_month month, day))
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

type tz_info = Time_zone.t * Duration.t option

let equal_tz_info (x : tz_info) (y : tz_info) =
  match (x, y) with
  | (tz1, None), (tz2, None) -> Time_zone.equal tz1 tz2
  | (tz1, Some x1), (tz2, Some x2) ->
    Time_zone.equal tz1 tz2 && Duration.equal x1 x2
  | _, _ -> false

let make_tz_info ?tz ?tz_offset () : tz_info option =
  match (tz, tz_offset) with
  | None, None -> invalid_arg "make_tz_info"
  | Some tz, None -> Some (tz, Time_zone.to_fixed_offset tz)
  | None, Some tz_offset ->
    Some (Time_zone.make_offset_only tz_offset, Some tz_offset)
  | Some tz, Some tz_offset ->
    if Time_zone.offset_is_recorded tz_offset tz then Some (tz, Some tz_offset)
    else None

let tz_offset_of_tz_info ((tz, tz_offset) : tz_info) =
  match tz_offset with Some x -> Some x | None -> Time_zone.to_fixed_offset tz

let next_ymd ~year ~month ~day : (int * month * int) option =
  let day_count = day_count_of_month ~year ~month in
  if 1 <= day && day <= day_count then
    if day < day_count then Some (year, month, succ day)
    else
      let month_int = tm_int_of_month month in
      let next_month_int = succ month_int in
      let year = year + (next_month_int / 12) in
      let month = CCOpt.get_exn @@ month_of_tm_int @@ (next_month_int mod 12) in
      Some (year, month, 1)
  else None
