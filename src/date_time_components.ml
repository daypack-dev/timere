open Int64_utils

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
  (weekday, unit) result =
  match Ptime.(of_date (year, human_int_of_month month, mday)) with
  | None -> Error ()
  | Some wday -> Ok (Ptime.weekday wday)

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

let tz_info_equal (x : tz_info) (y : tz_info) =
  match (x, y) with
  | `Tz_only x, `Tz_only y -> Time_zone.equal x y
  | `Tz_offset_s_only x, `Tz_offset_s_only y -> x = y
  | `Tz_and_tz_offset_s (tz1, x1), `Tz_and_tz_offset_s (tz2, x2) ->
    Time_zone.equal tz1 tz2 && x1 = x2
  | _, _ -> false

let make_tz_info ?tz ?tz_offset_s () =
  match (tz, tz_offset_s) with
  | None, None -> invalid_arg "make_tz_info"
  | Some tz, None -> Ok (`Tz_only tz)
  | None, Some tz_offset_s -> Ok (`Tz_offset_s_only tz_offset_s)
  | Some tz, Some tz_offset_s ->
    if Time_zone.offset_is_recorded tz_offset_s tz then
      Ok (`Tz_and_tz_offset_s (tz, tz_offset_s))
    else Error ()

let ptime_of_timestamp x =
  let d, s =
    if x >= 0L then
      ( x /^ Constants.seconds_in_day |> Int64.to_int,
        Int64.rem (Int64.abs x) Constants.seconds_in_day )
    else
      let x = Int64.abs x in
      let s = Int64.rem x Constants.seconds_in_day in
      ( -1
         * ((x +^ (Constants.seconds_in_day -^ 1L))
            /^ Constants.seconds_in_day
            |> Int64.to_int),
        if s = 0L then s else Constants.seconds_in_day -^ s )
  in
  let ps = s *^ Constants.s_to_ps_mult in
  match Ptime.Span.of_d_ps (d, ps) with
  | None -> Error ()
  | Some span -> (
      match Ptime.of_span span with
      | None -> Error ()
      | Some x -> Ok x
    )

let timestamp_of_ptime x =
  let d, ps = x |> Ptime.to_span |> Ptime.Span.to_d_ps in
  let s = ps /^ Constants.s_to_ps_mult in
  ((Int64.of_int d *^ Constants.seconds_in_day) +^ s)
