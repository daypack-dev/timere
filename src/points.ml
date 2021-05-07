open Timedesc

type pick =
  | S of int
  | MS of {
      minute : int;
      second : int;
    }
  | HMS of {
      hour : int;
      minute : int;
      second : int;
    }
  | WHMS of {
      weekday : weekday;
      hour : int;
      minute : int;
      second : int;
    }
  | DHMS of {
      month_day : int;
      hour : int;
      minute : int;
      second : int;
    }
  | MDHMS of {
      month : int;
      month_day : int;
      hour : int;
      minute : int;
      second : int;
    }
  | YMDHMS of {
      year : int;
      month : int;
      month_day : int;
      hour : int;
      minute : int;
      second : int;
    }

type t = pick * tz_info option

type error =
  [ `Invalid_year of int
  | `Invalid_day of int
  | `Invalid_hour of int
  | `Invalid_minute of int
  | `Invalid_second of int
  | `Invalid_pattern_combination
  | `Invalid_tz_info of string option * Span.t
  ]

exception Error_exn of error

let precision ((pick, _) : t) : int =
  match pick with
  | S _ -> 0
  | MS _ -> 1
  | HMS _ -> 2
  | WHMS _ -> 3
  | DHMS _ -> 4
  | MDHMS _ -> 5
  | YMDHMS _ -> 6

let make ?tz ?tz_offset ?year ?month ?day ?weekday ?hour ?minute ~second () :
  (t, error) result =
  let tz_info =
    match make_tz_info ?tz ?tz_offset () with
    | Ok tz_info -> Ok (Some tz_info)
    | Error `Missing_both_tz_and_tz_offset -> Ok None
    | Error (`Invalid_offset tz_offset) | Error (`Unrecorded_offset tz_offset)
      ->
      Error (`Invalid_tz_info (CCOpt.map Time_zone.name tz, tz_offset))
  in
  match tz_info with
  | Error e -> Error e
  | Ok tz_info ->
    let year_is_fine =
      match year with
      | None -> true
      | Some year ->  <= year && year <= Constants.max_year
    in
    let month_day_is_fine =
      match day with None -> true | Some x -> -31 <= x && x <= 31 && x <> 0
    in
    let hour_is_fine =
      match hour with None -> true | Some x -> 0 <= x && x < 24
    in
    let minute_is_fine =
      match minute with None -> true | Some x -> 0 <= x && x < 60
    in
    let second_is_fine = 0 <= second && second < 60 in
    if not year_is_fine then Error (`Invalid_year (CCOpt.get_exn year))
    else if not month_day_is_fine then
      Error (`Invalid_day (CCOpt.get_exn day))
    else if not hour_is_fine then Error (`Invalid_hour (CCOpt.get_exn hour))
    else if not minute_is_fine then
      Error (`Invalid_minute (CCOpt.get_exn minute))
    else if not second_is_fine then Error (`Invalid_second second)
    else
      let pick =
        match (year, month, day, weekday, hour, minute) with
        | None, None, None, None, None, None -> Ok (S second)
        | None, None, None, None, None, Some minute ->
          Ok (MS { minute; second })
        | None, None, None, None, Some hour, Some minute ->
          Ok (HMS { hour; minute; second })
        | None, None, None, Some weekday, Some hour, Some minute ->
          Ok (WHMS { weekday; hour; minute; second })
        | None, None, Some month_day, None, Some hour, Some minute ->
          Ok (DHMS { month_day; hour; minute; second })
        | None, Some month, Some month_day, None, Some hour, Some minute ->
          Ok (MDHMS { month; month_day; hour; minute; second })
        | Some year, Some month, Some month_day, None, Some hour, Some minute
          ->
          Ok (YMDHMS { year; month; month_day; hour; minute; second })
        | _ -> Error `Invalid_pattern_combination
      in
      CCResult.map (fun pick -> (pick, tz_info)) pick

let make_exn ?tz ?tz_offset ?year ?month ?day ?weekday ?hour ?minute ~second ()
  =
  match
    make ?tz ?tz_offset ?year ?month ?day ?weekday ?hour ?minute ~second ()
  with
  | Error e -> raise (Error_exn e)
  | Ok x -> x

let equal_pick t1 t2 =
  match (t1, t2) with
  | S x1, S x2 -> x1 = x2
  | MS { minute = m1; second = s1 }, MS { minute = m2; second = s2 } ->
    m1 = m2 && s1 = s2
  | ( HMS { hour = h1; minute = m1; second = s1 },
      HMS { hour = h2; minute = m2; second = s2 } ) ->
    h1 = h2 && m1 = m2 && s1 = s2
  | ( DHMS { month_day = day1; hour = h1; minute = m1; second = s1 },
      DHMS { month_day = day2; hour = h2; minute = m2; second = s2 } ) ->
    day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2
  | ( WHMS { weekday = day1; hour = h1; minute = m1; second = s1 },
      WHMS { weekday = day2; hour = h2; minute = m2; second = s2 } ) ->
    day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2
  | ( MDHMS
        { month = mon1; month_day = day1; hour = h1; minute = m1; second = s1 },
      MDHMS
        { month = mon2; month_day = day2; hour = h2; minute = m2; second = s2 }
    ) ->
    mon1 = mon2 && day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2
  | ( YMDHMS
        {
          year = y1;
          month = mon1;
          month_day = day1;
          hour = h1;
          minute = m1;
          second = s1;
        },
      YMDHMS
        {
          year = y2;
          month = mon2;
          month_day = day2;
          hour = h2;
          minute = m2;
          second = s2;
        } ) ->
    y1 = y2 && mon1 = mon2 && day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2
  | _, _ -> false

let equal (pick1, tz_info1) (pick2, tz_info2) =
  equal_pick pick1 pick2 && CCOpt.equal equal_tz_info tz_info1 tz_info2

let to_pattern (t, _tz_info) =
  let years =
    match t with
    | YMDHMS { year; _ } -> Int_set.add year Int_set.empty
    | _ -> Int_set.empty
  in
  let months =
    match t with
    | YMDHMS { month; _ } | MDHMS { month; _ } ->
      Int_set.add month Int_set.empty
    | _ -> Int_set.empty
  in
  let month_days =
    match t with
    | YMDHMS { month_day; _ } | MDHMS { month_day; _ } | DHMS { month_day; _ }
      ->
      Int_set.add month_day Int_set.empty
    | _ -> Int_set.empty
  in
  let weekdays =
    match t with
    | WHMS { weekday; _ } -> Weekday_set.add weekday Weekday_set.empty
    | _ -> Weekday_set.empty
  in
  let hours =
    match t with
    | YMDHMS { hour; _ }
    | MDHMS { hour; _ }
    | WHMS { hour; _ }
    | DHMS { hour; _ }
    | HMS { hour; _ } ->
      Int_set.add hour Int_set.empty
    | _ -> Int_set.empty
  in
  let minutes =
    match t with
    | YMDHMS { minute; _ }
    | MDHMS { minute; _ }
    | WHMS { minute; _ }
    | DHMS { minute; _ }
    | HMS { minute; _ }
    | MS { minute; _ } ->
      Int_set.add minute Int_set.empty
    | _ -> Int_set.empty
  in
  let seconds =
    match t with
    | YMDHMS { second; _ }
    | MDHMS { second; _ }
    | WHMS { second; _ }
    | DHMS { second; _ }
    | HMS { second; _ }
    | MS { second; _ }
    | S second ->
      Int_set.add second Int_set.empty
  in
  Pattern.{ years; months; month_days; weekdays; hours; minutes; seconds }
