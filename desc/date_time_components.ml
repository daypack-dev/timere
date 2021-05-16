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

(** Implementation copied from Ptime

    Reference quoted in Ptime:
    https://www.tondering.dk/claus/cal/julperiod.php#formula
*)

let jd_of_ymd ~year ~month ~day =
  let a = (14 - month) / 12 in
  let y = year + 4800 - a in
  let m = month + (12 * a) - 3 in
  day
  + (((153 * m) + 2) / 5)
  + (365 * y)
  + (y / 4)
  - (y / 100)
  + (y / 400)
  - 32045

let ymd_of_jd jd =
  let a = jd + 32044 in
  let b = ((4 * a) + 3) / 146097 in
  let c = a - (146097 * b / 4) in
  let d = ((4 * c) + 3) / 1461 in
  let e = c - (1461 * d / 4) in
  let m = ((5 * e) + 2) / 153 in
  let day = e - (((153 * m) + 2) / 5) + 1 in
  let month = m + 3 - (12 * (m / 10)) in
  let year = (100 * b) + d - 4800 + (m / 10) in
  (year, month, day)

let jd_of_epoch = jd_of_ymd ~year:1970 ~month:1 ~day:1

let weekday_of_jd jd =
  (* epoch was on thursday *)
  let d = (jd - jd_of_epoch) mod 7 in
  match if d < 0 then d + 7 else d with
  | 0 -> `Thu
  | 1 -> `Fri
  | 2 -> `Sat
  | 3 -> `Sun
  | 4 -> `Mon
  | 5 -> `Tue
  | 6 -> `Wed
  | _ -> failwith "Unexpected case"

let weekday_of_ymd ~year ~month ~day =
  weekday_of_jd (jd_of_ymd ~year ~month ~day)

let is_leap_year ~year =
  let divisible_by_4 = year mod 4 = 0 in
  let divisible_by_100 = year mod 100 = 0 in
  let divisible_by_400 = year mod 400 = 0 in
  divisible_by_4 && ((not divisible_by_100) || divisible_by_400)

let day_count_of_year ~year = if is_leap_year ~year then 366 else 365

let day_offset_from_start_of_year ~year ~month =
  let aux month =
    match month with
    | 1 -> 0
    | 2 -> 31
    | 3 -> 59
    | 4 -> 90
    | 5 -> 120
    | 6 -> 151
    | 7 -> 181
    | 8 -> 212
    | 9 -> 243
    | 10 -> 273
    | 11 -> 304
    | 12 -> 334
    | _ -> failwith "Unexpected case"
  in
  if is_leap_year ~year then if month > 2 then aux month + 1 else aux month
  else aux month

let md_of_ydoy ~year ~day_of_year : int * int =
  let rec aux ~month =
    if month < 1 then failwith "Unexpected case"
    else
      let offset = day_offset_from_start_of_year ~year ~month in
      if offset + 1 <= day_of_year then month else aux ~month:(pred month)
  in
  let month = aux ~month:12 in
  let day = day_of_year - day_offset_from_start_of_year ~year ~month in
  (month, day)

let doy_of_ymd ~year ~month ~day : int =
  day_offset_from_start_of_year ~year ~month + day

let jd_of_ydoy ~year ~day_of_year =
  let month, day = md_of_ydoy ~year ~day_of_year in
  jd_of_ymd ~year ~month ~day

let jd_of_start_of_iso_week_year ~iso_week_year =
  (* we use the fact that
     - Jan 4th is always in week 1 of the year
     - and week starts on Monday

     to find the start date of week 1
  *)
  match weekday_of_ymd ~year:iso_week_year ~month:1 ~day:4 with
  | `Mon -> jd_of_ymd ~year:iso_week_year ~month:1 ~day:4
  | `Tue -> jd_of_ymd ~year:iso_week_year ~month:1 ~day:3
  | `Wed -> jd_of_ymd ~year:iso_week_year ~month:1 ~day:2
  | `Thu -> jd_of_ymd ~year:iso_week_year ~month:1 ~day:1
  | `Fri -> jd_of_ymd ~year:(pred iso_week_year) ~month:12 ~day:31
  | `Sat -> jd_of_ymd ~year:(pred iso_week_year) ~month:12 ~day:30
  | `Sun -> jd_of_ymd ~year:(pred iso_week_year) ~month:12 ~day:29

let week_count_of_iso_week_year ~iso_week_year =
  (jd_of_start_of_iso_week_year ~iso_week_year:(succ iso_week_year)
   - jd_of_start_of_iso_week_year ~iso_week_year)
  / 7

let iso_int_of_weekday (weekday : weekday) =
  match weekday with
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6
  | `Sun -> 7

let weekday_of_iso_int x =
  match x with
  | 1 -> Some `Mon
  | 2 -> Some `Tue
  | 3 -> Some `Wed
  | 4 -> Some `Thu
  | 5 -> Some `Fri
  | 6 -> Some `Sat
  | 7 -> Some `Sun
  | _ -> None

let iso_week_date_of_jd (jd : int) : int * int * weekday =
  let year, month, day = ymd_of_jd jd in
  let day_of_year = doy_of_ymd ~year ~month ~day in
  let weekday = weekday_of_jd jd in
  let week_of_year = (10 + day_of_year - iso_int_of_weekday weekday) / 7 in
  assert (week_of_year >= 0);
  assert (week_of_year <= 53);
  let iso_week_year, week =
    if week_of_year = 0 then
      (pred year, week_count_of_iso_week_year ~iso_week_year:(pred year))
    else if
      week_of_year = 53 && week_count_of_iso_week_year ~iso_week_year:year < 53
    then (succ year, 1)
    else (year, week_of_year)
  in
  (iso_week_year, week, weekday)

let jd_of_iso_week_date ~iso_week_year ~iso_week ~weekday =
  let weekday_int = iso_int_of_weekday weekday in
  let jan_4_weekday_int =
    iso_int_of_weekday (weekday_of_ymd ~year:iso_week_year ~month:1 ~day:4)
  in
  let day_of_year = (iso_week * 7) + weekday_int - (jan_4_weekday_int + 3) in
  let day_count_of_prev_year = day_count_of_year ~year:(pred iso_week_year) in
  let day_count_of_cur_year = day_count_of_year ~year:iso_week_year in
  let year, day_of_year =
    if day_of_year < 1 then
      (pred iso_week_year, day_count_of_prev_year + day_of_year)
    else if day_of_year > day_count_of_cur_year then
      (succ iso_week_year, day_of_year - day_count_of_cur_year)
    else (iso_week_year, day_of_year)
  in
  jd_of_ydoy ~year ~day_of_year

module Weekday_set = struct
  include CCSet.Make (struct
      type t = weekday

      let compare = compare_weekday
    end)

  let to_seq x = x |> to_list |> CCList.to_seq
end

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

let full_string_of_weekday (wday : weekday) : string =
  match wday with
  | `Sun -> "Sunday"
  | `Mon -> "Monday"
  | `Tue -> "Tuesday"
  | `Wed -> "Wednesday"
  | `Thu -> "Thursday"
  | `Fri -> "Friday"
  | `Sat -> "Saturday"

let weekday_of_full_string s : weekday option =
  match s with
  | "Sunday" -> Some `Sun
  | "Monday" -> Some `Mon
  | "Tuesday" -> Some `Tue
  | "Wednesday" -> Some `Wed
  | "Thursday" -> Some `Thu
  | "Friday" -> Some `Fri
  | "Saturday" -> Some `Sat
  | _ -> None

let abbr_string_of_weekday (wday : weekday) : string =
  String.sub (full_string_of_weekday wday) 0 3

let weekday_of_abbr_string s : weekday option =
  match s with
  | "Sun" -> Some `Sun
  | "Mon" -> Some `Mon
  | "Tue" -> Some `Tue
  | "Wed" -> Some `Wed
  | "Thu" -> Some `Thu
  | "Fri" -> Some `Fri
  | "Sat" -> Some `Sat
  | _ -> None

let full_string_of_month (month : int) : string option =
  match month with
  | 1 -> Some "January"
  | 2 -> Some "February"
  | 3 -> Some "March"
  | 4 -> Some "April"
  | 5 -> Some "May"
  | 6 -> Some "June"
  | 7 -> Some "July"
  | 8 -> Some "August"
  | 9 -> Some "September"
  | 10 -> Some "October"
  | 11 -> Some "November"
  | 12 -> Some "December"
  | _ -> None

let month_of_full_string s : int option =
  match s with
  | "January" -> Some 1
  | "February" -> Some 2
  | "March" -> Some 3
  | "April" -> Some 4
  | "May" -> Some 5
  | "June" -> Some 6
  | "July" -> Some 7
  | "August" -> Some 8
  | "September" -> Some 9
  | "October" -> Some 10
  | "November" -> Some 11
  | "December" -> Some 12
  | _ -> None

let abbr_string_of_month (month : int) : string option =
  CCOpt.map (fun s -> String.sub s 0 3) (full_string_of_month month)

let month_of_abbr_string s : int option =
  match s with
  | "Jan" -> Some 1
  | "Feb" -> Some 2
  | "Mar" -> Some 3
  | "Apr" -> Some 4
  | "May" -> Some 5
  | "Jun" -> Some 6
  | "Jul" -> Some 7
  | "Aug" -> Some 8
  | "Sep" -> Some 9
  | "Oct" -> Some 10
  | "Nov" -> Some 11
  | "Dec" -> Some 12
  | _ -> None
