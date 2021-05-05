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
  let m = month + 12 * a - 3 in
  day + ((153 * m) + 2)/ 5 + 365 * y +
  (y / 4) - (y / 100) + (y / 400) - 32045

let ymd_of_jd jd =
  let a = jd + 32044 in
  let b = (4 * a + 3) / 146097 in
  let c = a - ((146097 * b) / 4) in
  let d = (4 * c + 3) / 1461 in
  let e = c - ((1461 * d) / 4) in
  let m = (5 * e + 2) / 153 in
  let day = e - ((153 * m + 2) / 5) + 1 in
  let month = m + 3 - (12 * (m / 10)) in
  let year = 100 * b + d - 4800 + (m / 10) in
  (year, month, day)

let jd_of_epoch =
  jd_of_ymd ~year:1970 ~month:1 ~day:1

let weekday_of_ymd ~year ~month ~day =
  (* epoch was on thursday *)
  let d = (jd_of_ymd ~year ~month ~day - jd_of_epoch) mod 7 in
  match if d < 0 then d + 7 else d with
  | 0 -> `Thu
  | 1 -> `Fri
  | 2 -> `Sat
  | 3 -> `Sun
  | 4 -> `Mon
  | 5 -> `Tue
  | 6 -> `Wed
  | _ -> failwith "Unexpected case"

let is_leap_year ~year =
  assert (year >= 0);
  let divisible_by_4 = year mod 4 = 0 in
  let divisible_by_100 = year mod 100 = 0 in
  let divisible_by_400 = year mod 400 = 0 in
  divisible_by_4 && ((not divisible_by_100) || divisible_by_400)

let day_count_of_year ~year = if is_leap_year ~year then 366 else 365

let day_offset_from_start_of_year ~year ~month =
  let offset_from_leap_year =
    if is_leap_year ~year:year then 1 else 0
  in
  match month with
  | 1 -> 0
  | 2 -> 31
  | 3 -> 59 + offset_from_leap_year
  | 4 -> 90 + offset_from_leap_year
  | 5 -> 120 + offset_from_leap_year
  | 6 -> 151 + offset_from_leap_year
  | 7 -> 181 + offset_from_leap_year
  | 8 -> 212 + offset_from_leap_year
  | 9 -> 243 + offset_from_leap_year
  | 10 -> 273 + offset_from_leap_year
  | 11 -> 304 + offset_from_leap_year
  | 12 -> 334 + offset_from_leap_year
  | _ -> failwith "Unexpected case"

let md_of_ydoy ~year ~day_of_year : (int * int) =
  let rec aux ~month =
    if month < 1 then
      failwith "Unexpected case"
    else
      let offset = day_offset_from_start_of_year ~year ~month in
      if offset + 1 <= day_of_year then
        month
      else
        aux ~month:(pred month)
  in
  let month = aux ~month:12 in
  let day = day_of_year - day_offset_from_start_of_year ~year ~month in
  (month, day)

let doy_of_ymd ~year ~month ~day : int =
  day_offset_from_start_of_year ~year ~month + day

module Weekday_set = struct
  include CCSet.Make (struct
      type t = weekday

      let compare = compare_weekday
    end)

  let to_seq x = x |> to_list |> CCList.to_seq
end

let weekday_of_ymd ~(year : int) ~(month : int) ~(day : int) :
  weekday option =
  Ptime.(of_date (year, month, day)) |> CCOpt.map Ptime.weekday

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

type tz_info = {
  tz : Time_zone.t;
  offset : Span.t option;
}

let equal_tz_info (x : tz_info) (y : tz_info) =
  match (x, y) with
  | { tz = tz1; offset = None }, { tz = tz2; offset = None } ->
    Time_zone.equal tz1 tz2
  | { tz = tz1; offset = Some x1 }, { tz = tz2; offset = Some x2 } ->
    Time_zone.equal tz1 tz2 && Span.equal x1 x2
  | _, _ -> false

type tz_info_error =
  [ `Missing_both_tz_and_tz_offset
  | `Invalid_offset of Span.t
  | `Unrecorded_offset of Span.t
  ]

let make_tz_info ?tz ?tz_offset () : (tz_info, tz_info_error) result =
  match (tz, tz_offset) with
  | None, None -> Error `Missing_both_tz_and_tz_offset
  | Some tz, None -> Ok { tz; offset = Time_zone.to_fixed_offset tz }
  | None, Some tz_offset -> (
      match Time_zone.make_offset_only tz_offset with
      | None -> Error (`Invalid_offset tz_offset)
      | Some tz -> Ok { tz; offset = Some tz_offset })
  | Some tz, Some tz_offset ->
    if Time_zone.offset_is_recorded tz_offset tz then
      Ok { tz; offset = Some tz_offset }
    else Error (`Unrecorded_offset tz_offset)

let tz_offset_of_tz_info ({ tz; offset } : tz_info) =
  match offset with Some x -> Some x | None -> Time_zone.to_fixed_offset tz
