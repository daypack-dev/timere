open Date_components

type pick =
  | S of int
  | MS of { minute : int; second : int }
  | HMS of { hour : int; minute : int; second : int }
  | WHMS of { weekday : weekday; hour : int; minute : int; second : int }
  | DHMS of { month_day : int; hour : int; minute : int; second : int }
  | MDHMS of { month : month; month_day : int; hour : int; minute : int; second : int }
  | YMDHMS of { year : int; month : month; month_day : int; hour : int; minute : int; second : int }

type t = pick * tz_info option

let make ?tz_info ?year ?month ?month_day ?weekday ?hour ?minute ~second () =
  let pick =
    match year, month, month_day, weekday, hour, minute with
    | None, None, None, None, None, None ->
      S second
    | None, None, None, None, None, Some minute ->
      MS { minute; second }
    | None, None, None, None, Some hour, Some minute ->
      HMS { hour; minute; second }
    | None, None, None, Some weekday, Some hour, Some minute ->
      WHMS { weekday; hour; minute; second }
    | None, None, Some month_day, None, Some hour, Some minute ->
      DHMS { month_day; hour; minute; second }
    | None, Some month, Some month_day, None, Some hour, Some minute ->
      MDHMS { month; month_day; hour; minute; second }
    | Some year, Some month, Some month_day, None, Some hour, Some minute ->
      YMDHMS { year; month; month_day; hour; minute; second }
    | _ -> invalid_arg "make"
  in
  ( pick, tz_info)

let pick_equal t1 t2 =
  match t1, t2 with
  | S x1, S x2 -> x1 = x2
  | MS { minute = m1; second = s1 }, MS { minute = m2; second = s2 } ->
    m1 = m2 && s1 = s2
  | HMS { hour = h1; minute = m1; second = s1 }, HMS { hour = h2; minute = m2; second = s2 } ->
    h1 = h2 && m1 = m2 && s1 = s2
  | DHMS { month_day = day1; hour = h1; minute = m1; second = s1 }, DHMS { month_day = day2; hour = h2; minute = m2; second = s2 } ->
    day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2
  | WHMS { weekday = day1; hour = h1; minute = m1; second = s1 }, WHMS { weekday = day2; hour = h2; minute = m2; second = s2 } ->
    day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2
  | MDHMS { month = mon1; month_day = day1; hour = h1; minute = m1; second = s1 },
    MDHMS { month = mon2; month_day = day2; hour = h2; minute = m2; second = s2 } ->
    mon1 = mon2 && day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2
  | YMDHMS { year = y1; month = mon1; month_day = day1; hour = h1; minute = m1; second = s1 },
    YMDHMS { year = y2; month = mon2; month_day = day2; hour = h2; minute = m2; second = s2 } ->
    y1 = y2 && mon1 = mon2 && day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2
  | _, _ -> false

let equal (pick1, tz_info1) (pick2, tz_info2) =
  pick_equal pick1 pick2
  &&
  CCOpt.equal tz_info_equal tz_info1 tz_info2

let to_pattern t =
  let years =
    match t with
    | YMDHMS { year; _ } -> Int_set.add year Int_set.empty
    | _ -> Int_set.empty
  in
  let months =
    match t with
    | YMDHMS { month; _ }
    | MDHMS { month; _ } -> Month_set.add month Month_set.empty
    | _ -> Month_set.empty
  in
  let month_days =
    match t with
    | YMDHMS { month_day; _ }
    | MDHMS { month_day; _ }
    | DHMS { month_day; _ } -> Int_set.add month_day Int_set.empty
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
    | HMS { hour; _ } -> Int_set.add hour Int_set.empty
    | _ -> Int_set.empty
  in
  let minutes =
    match t with
    | YMDHMS { minute; _ }
    | MDHMS { minute; _ }
    | WHMS { minute; _ }
    | DHMS { minute; _ }
    | HMS { minute; _ }
    | MS { minute; _ }
      -> Int_set.add minute Int_set.empty
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
      -> Int_set.add second Int_set.empty
    | _ -> Int_set.empty
  in
  Pattern.{
    years; months; month_days; weekdays; hours; minutes; seconds
  }
