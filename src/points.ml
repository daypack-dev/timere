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
      weekday : Timedesc.weekday;
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

type t = {
  pick : pick;
  tz_info : Timedesc.Time_zone_info.t option;
}

type error =
  [ `Invalid_year of int
  | `Invalid_day of int
  | `Invalid_hour of int
  | `Invalid_minute of int
  | `Invalid_second of int
  | `Invalid_pattern_combination
  | `Invalid_tz_info of string option * Timedesc.Span.t
  ]

exception Error_exn of error

let precision ({ pick; _ } : t) : int =
  match pick with
  | S _ -> 0
  | MS _ -> 1
  | HMS _ -> 2
  | WHMS _ -> 3
  | DHMS _ -> 4
  | MDHMS _ -> 5
  | YMDHMS _ -> 6

let make ?tz ?offset_from_utc ?year ?month ?day ?weekday ?hour ?minute ~second
    () : (t, error) result =
  let tz_info =
    match
      Timedesc.Time_zone_info.make ?tz ?fixed_offset_from_utc:offset_from_utc ()
    with
    | Ok tz_info -> Ok (Some tz_info)
    | Error `Missing_both_tz_and_fixed_offset_from_utc -> Ok None
    | Error (`Invalid_offset tz_offset) | Error (`Unrecorded_offset tz_offset)
      ->
      Error
        (`Invalid_tz_info (CCOpt.map Timedesc.Time_zone.name tz, tz_offset))
  in
  match tz_info with
  | Error e -> Error e
  | Ok tz_info ->
    let year_is_fine =
      match year with
      | None -> true
      | Some year ->
        Timedesc.(year min_val) <= year && year <= Timedesc.(year max_val)
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
    if not year_is_fine then
      Error
        (`Invalid_year (CCOpt.get_exn_or "Expected year to be Some _" year))
    else if not month_day_is_fine then
      Error (`Invalid_day (CCOpt.get_exn_or "Expected day to be Some _" day))
    else if not hour_is_fine then
      Error
        (`Invalid_hour (CCOpt.get_exn_or "Expected hour to be Some _" hour))
    else if not minute_is_fine then
      Error
        (`Invalid_minute
           (CCOpt.get_exn_or "Expected minute to be Some _" minute))
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
      CCResult.map (fun pick -> { pick; tz_info }) pick

let make_exn ?tz ?offset_from_utc ?year ?month ?day ?weekday ?hour ?minute
    ~second () =
  match
    make ?tz ?offset_from_utc ?year ?month ?day ?weekday ?hour ?minute ~second
      ()
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

let equal x y =
  equal_pick x.pick y.pick
  && CCOpt.equal Timedesc.Time_zone_info.equal x.tz_info y.tz_info

let to_pattern ({ pick; tz_info = _ } : t) =
  let years =
    match pick with
    | YMDHMS { year; _ } -> Int_set.add year Int_set.empty
    | _ -> Int_set.empty
  in
  let months =
    match pick with
    | YMDHMS { month; _ } | MDHMS { month; _ } ->
      Int_set.add month Int_set.empty
    | _ -> Int_set.empty
  in
  let month_days =
    match pick with
    | YMDHMS { month_day; _ } | MDHMS { month_day; _ } | DHMS { month_day; _ }
      ->
      Int_set.add month_day Int_set.empty
    | _ -> Int_set.empty
  in
  let weekdays =
    match pick with
    | WHMS { weekday; _ } -> Weekday_set.add weekday Weekday_set.empty
    | _ -> Weekday_set.empty
  in
  let hours =
    match pick with
    | YMDHMS { hour; _ }
    | MDHMS { hour; _ }
    | WHMS { hour; _ }
    | DHMS { hour; _ }
    | HMS { hour; _ } ->
      Int_set.add hour Int_set.empty
    | _ -> Int_set.empty
  in
  let minutes =
    match pick with
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
    match pick with
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

let to_date_time ~default_tz_info ({ pick; tz_info } : t) : Timedesc.t option =
  let tz_info = match tz_info with None -> default_tz_info | Some x -> x in
  match pick with
  | YMDHMS { year; month; month_day; hour; minute; second } -> (
      let tz = tz_info.tz in
      match tz_info.fixed_offset_from_utc with
      | Some offset_from_utc ->
        Some
          (Timedesc.make_unambiguous_exn ~tz ~offset_from_utc ~year ~month
             ~day:month_day ~hour ~minute ~second ())
      | None ->
        Some
          (Timedesc.make_exn ~tz ~year ~month ~day:month_day ~hour ~minute
             ~second ()))
  | _ -> None
