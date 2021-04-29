type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
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

let compare_weekday (d1 : weekday) (d2 : weekday) : int =
  compare (tm_int_of_weekday d1) (tm_int_of_weekday d2)

module Weekday_set = struct
  include CCSet.Make (struct
      type t = weekday

      let compare = compare_weekday
    end)

  let to_seq x = x |> to_list |> CCList.to_seq
end

let weekday_of_month_day ~(year : int) ~(month : int) ~(day : int) :
  weekday option =
  Ptime.(of_date (year, month, day))
  |> CCOpt.map Ptime.weekday

let is_leap_year ~year =
  assert (year >= 0);
  let divisible_by_4 = year mod 4 = 0 in
  let divisible_by_100 = year mod 100 = 0 in
  let divisible_by_400 = year mod 400 = 0 in
  divisible_by_4 && ((not divisible_by_100) || divisible_by_400)

let day_count_of_year ~year = if is_leap_year ~year then 366 else 365

let day_count_of_month ~year ~month =
  match month with
  | 1 -> 31
  | 2 -> if is_leap_year ~year then 29 else 28
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31
  | _ -> invalid_arg "Invalid month"

type tz_info = Time_zone.t * Duration.t option

let equal_tz_info (x : tz_info) (y : tz_info) =
  match (x, y) with
  | (tz1, None), (tz2, None) -> Time_zone.equal tz1 tz2
  | (tz1, Some x1), (tz2, Some x2) ->
    Time_zone.equal tz1 tz2 && Duration.equal x1 x2
  | _, _ -> false

type tz_info_error =
  [ `Missing_both_tz_and_tz_offset
  | `Invalid_offset of Duration.t
  | `Unrecorded_offset of Duration.t
  ]

let make_tz_info ?tz ?tz_offset () : (tz_info, tz_info_error) result =
  match (tz, tz_offset) with
  | None, None -> Error `Missing_both_tz_and_tz_offset
  | Some tz, None -> Ok (tz, Time_zone.to_fixed_offset tz)
  | None, Some tz_offset -> (
      match Time_zone.make_offset_only tz_offset with
      | None -> Error (`Invalid_offset tz_offset)
      | Some tz -> Ok (tz, Some tz_offset))
  | Some tz, Some tz_offset ->
    if Time_zone.offset_is_recorded tz_offset tz then Ok (tz, Some tz_offset)
    else Error (`Unrecorded_offset tz_offset)

let tz_offset_of_tz_info ((tz, tz_offset) : tz_info) =
  match tz_offset with Some x -> Some x | None -> Time_zone.to_fixed_offset tz
