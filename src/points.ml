type pick =
  | N of int
  | SN of {
      second : int;
      ns : int;
    }
  | MSN of {
      minute : int;
      second : int;
      ns : int;
    }
  | HMSN of {
      hour : int;
      minute : int;
      second : int;
      ns : int;
    }
  | WHMSN of {
      weekday : Timedesc.weekday;
      hour : int;
      minute : int;
      second : int;
      ns : int;
    }
  | DHMSN of {
      month_day : int;
      hour : int;
      minute : int;
      second : int;
      ns : int;
    }
  | MDHMSN of {
      month : int;
      month_day : int;
      hour : int;
      minute : int;
      second : int;
      ns : int;
    }
  | YMDHMSN of {
      year : int;
      month : int;
      month_day : int;
      hour : int;
      minute : int;
      second : int;
      ns : int;
    }

type t = {
  pick : pick;
  tz_info : Timedesc.Time_zone_info.t option;
}

type lean_toward =
  [ `Earlier
  | `Later
  ]

type error =
  [ `Invalid_year of int
  | `Invalid_day of int
  | `Invalid_hour of int
  | `Invalid_minute of int
  | `Invalid_second of int
  | `Invalid_ns of int
  | `Invalid_pattern_combination
  | `Invalid_tz_info of string option * Timedesc.Span.t
  ]

exception Error_exn of error

let precision ({ pick; _ } : t) : int =
  match pick with
  | N _ -> 0
  | SN _ -> 1
  | MSN _ -> 2
  | HMSN _ -> 3
  | WHMSN _ -> 4
  | DHMSN _ -> 5
  | MDHMSN _ -> 6
  | YMDHMSN _ -> 7

let make ?tz ?offset_from_utc ?year ?month ?day ?weekday ?hour ?minute ?second
    ?ns ~(lean_toward : lean_toward) () : (t, error) result =
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
      let second_is_fine =
        match second with None -> true | Some x -> 0 <= x && x < 60
      in
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
      else if not second_is_fine then
        Error
          (`Invalid_second
            (CCOpt.get_exn_or "Expected second to be Some _" second))
      else
        let default_month =
          match lean_toward with `Earlier -> 1 | `Later -> 12
        in
        let default_month_day =
          match lean_toward with `Earlier -> 1 | `Later -> -1
        in
        let default_hour =
          match lean_toward with `Earlier -> 0 | `Later -> 23
        in
        let default_minute =
          match lean_toward with `Earlier -> 0 | `Later -> 59
        in
        let default_second =
          match lean_toward with `Earlier -> 0 | `Later -> 59
        in
        let default_ns =
          match lean_toward with
          | `Earlier -> 0
          | `Later -> Timedesc.Span.ns_count_in_s - 1
        in
        let pick =
          match (year, month, day, weekday, hour, minute, second, ns) with
          | Some year, None, None, None, None, None, None, None ->
              Ok
                (YMDHMSN
                   {
                     year;
                     month = default_month;
                     month_day = default_month_day;
                     hour = default_hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | Some year, Some month, None, None, None, None, None, None ->
              Ok
                (YMDHMSN
                   {
                     year;
                     month;
                     month_day = default_month_day;
                     hour = default_hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | Some year, Some month, Some month_day, None, None, None, None, None
            ->
              Ok
                (YMDHMSN
                   {
                     year;
                     month;
                     month_day;
                     hour = default_hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | ( Some year,
              Some month,
              Some month_day,
              None,
              Some hour,
              None,
              None,
              None ) ->
              Ok
                (YMDHMSN
                   {
                     year;
                     month;
                     month_day;
                     hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | ( Some year,
              Some month,
              Some month_day,
              None,
              Some hour,
              Some minute,
              None,
              None ) ->
              Ok
                (YMDHMSN
                   {
                     year;
                     month;
                     month_day;
                     hour;
                     minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | ( Some year,
              Some month,
              Some month_day,
              None,
              Some hour,
              Some minute,
              Some second,
              None ) ->
              Ok
                (YMDHMSN
                   {
                     year;
                     month;
                     month_day;
                     hour;
                     minute;
                     second;
                     ns = default_ns;
                   })
          | ( Some year,
              Some month,
              Some month_day,
              None,
              Some hour,
              Some minute,
              Some second,
              Some ns ) ->
              Ok (YMDHMSN { year; month; month_day; hour; minute; second; ns })
          | None, Some month, None, None, None, None, None, None ->
              Ok
                (MDHMSN
                   {
                     month;
                     month_day = default_month_day;
                     hour = default_hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | None, Some month, Some month_day, None, None, None, None, None ->
              Ok
                (MDHMSN
                   {
                     month;
                     month_day;
                     hour = default_hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | None, Some month, Some month_day, None, Some hour, None, None, None
            ->
              Ok
                (MDHMSN
                   {
                     month;
                     month_day;
                     hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | ( None,
              Some month,
              Some month_day,
              None,
              Some hour,
              Some minute,
              None,
              None ) ->
              Ok
                (MDHMSN
                   {
                     month;
                     month_day;
                     hour;
                     minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | ( None,
              Some month,
              Some month_day,
              None,
              Some hour,
              Some minute,
              Some second,
              None ) ->
              Ok
                (MDHMSN
                   { month; month_day; hour; minute; second; ns = default_ns })
          | ( None,
              Some month,
              Some month_day,
              None,
              Some hour,
              Some minute,
              Some second,
              Some ns ) ->
              Ok (MDHMSN { month; month_day; hour; minute; second; ns })
          | None, None, Some month_day, None, None, None, None, None ->
              Ok
                (DHMSN
                   {
                     month_day;
                     hour = default_hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | None, None, Some month_day, None, Some hour, None, None, None ->
              Ok
                (DHMSN
                   {
                     month_day;
                     hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | None, None, Some month_day, None, Some hour, Some minute, None, None
            ->
              Ok
                (DHMSN
                   {
                     month_day;
                     hour;
                     minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | ( None,
              None,
              Some month_day,
              None,
              Some hour,
              Some minute,
              Some second,
              None ) ->
              Ok (DHMSN { month_day; hour; minute; second; ns = default_ns })
          | ( None,
              None,
              Some month_day,
              None,
              Some hour,
              Some minute,
              Some second,
              Some ns ) ->
              Ok (DHMSN { month_day; hour; minute; second; ns })
          | None, None, None, Some weekday, None, None, None, None ->
              Ok
                (WHMSN
                   {
                     weekday;
                     hour = default_hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | None, None, None, Some weekday, Some hour, None, None, None ->
              Ok
                (WHMSN
                   {
                     weekday;
                     hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | None, None, None, Some weekday, Some hour, Some minute, None, None
            ->
              Ok
                (WHMSN
                   {
                     weekday;
                     hour;
                     minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | ( None,
              None,
              None,
              Some weekday,
              Some hour,
              Some minute,
              Some second,
              None ) ->
              Ok (WHMSN { weekday; hour; minute; second; ns = default_ns })
          | ( None,
              None,
              None,
              Some weekday,
              Some hour,
              Some minute,
              Some second,
              Some ns ) ->
              Ok (WHMSN { weekday; hour; minute; second; ns })
          | None, None, None, None, Some hour, None, None, None ->
              Ok
                (HMSN
                   {
                     hour;
                     minute = default_minute;
                     second = default_second;
                     ns = default_ns;
                   })
          | None, None, None, None, Some hour, Some minute, None, None ->
              Ok
                (HMSN { hour; minute; second = default_second; ns = default_ns })
          | None, None, None, None, Some hour, Some minute, Some second, None ->
              Ok (HMSN { hour; minute; second; ns = default_ns })
          | None, None, None, None, Some hour, Some minute, Some second, Some ns
            ->
              Ok (HMSN { hour; minute; second; ns })
          | None, None, None, None, None, Some minute, None, None ->
              Ok (MSN { minute; second = default_second; ns = default_ns })
          | None, None, None, None, None, Some minute, Some second, None ->
              Ok (MSN { minute; second; ns = default_ns })
          | None, None, None, None, None, Some minute, Some second, Some ns ->
              Ok (MSN { minute; second; ns })
          | None, None, None, None, None, None, Some second, None ->
              Ok (SN { second; ns = default_ns })
          | None, None, None, None, None, None, Some second, Some ns ->
              Ok (SN { second; ns })
          | None, None, None, None, None, None, None, Some ns -> Ok (N ns)
          | _, _, _, _, _, _, _, _ -> Error `Invalid_pattern_combination
        in
        CCResult.map (fun pick -> { pick; tz_info }) pick

let make_exn ?tz ?offset_from_utc ?year ?month ?day ?weekday ?hour ?minute
    ?second ?ns ~lean_toward () =
  match
    make ?tz ?offset_from_utc ?year ?month ?day ?weekday ?hour ?minute ?second
      ?ns ~lean_toward ()
  with
  | Error e -> raise (Error_exn e)
  | Ok x -> x

let equal_pick t1 t2 =
  match (t1, t2) with
  | N x1, N x2 -> x1 = x2
  | SN { second = s1; ns = ns1 }, SN { second = s2; ns = ns2 } ->
      s1 = s2 && ns1 = ns2
  | ( MSN { minute = m1; second = s1; ns = ns1 },
      MSN { minute = m2; second = s2; ns = ns2 } ) ->
      m1 = m2 && s1 = s2 && ns1 = ns2
  | ( HMSN { hour = h1; minute = m1; second = s1; ns = ns1 },
      HMSN { hour = h2; minute = m2; second = s2; ns = ns2 } ) ->
      h1 = h2 && m1 = m2 && s1 = s2 && ns1 = ns2
  | ( DHMSN { month_day = day1; hour = h1; minute = m1; second = s1; ns = ns1 },
      DHMSN { month_day = day2; hour = h2; minute = m2; second = s2; ns = ns2 }
    ) ->
      day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2 && ns1 = ns2
  | ( WHMSN { weekday = day1; hour = h1; minute = m1; second = s1; ns = ns1 },
      WHMSN { weekday = day2; hour = h2; minute = m2; second = s2; ns = ns2 } )
    ->
      day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2 && ns1 = ns2
  | ( MDHMSN
        {
          month = mon1;
          month_day = day1;
          hour = h1;
          minute = m1;
          second = s1;
          ns = ns1;
        },
      MDHMSN
        {
          month = mon2;
          month_day = day2;
          hour = h2;
          minute = m2;
          second = s2;
          ns = ns2;
        } ) ->
      mon1 = mon2 && day1 = day2 && h1 = h2 && m1 = m2 && s1 = s2 && ns1 = ns2
  | ( YMDHMSN
        {
          year = y1;
          month = mon1;
          month_day = day1;
          hour = h1;
          minute = m1;
          second = s1;
          ns = ns1;
        },
      YMDHMSN
        {
          year = y2;
          month = mon2;
          month_day = day2;
          hour = h2;
          minute = m2;
          second = s2;
          ns = ns2;
        } ) ->
      y1 = y2
      && mon1 = mon2
      && day1 = day2
      && h1 = h2
      && m1 = m2
      && s1 = s2
      && ns1 = ns2
  | _, _ -> false

let equal x y =
  equal_pick x.pick y.pick
  && CCOpt.equal Timedesc.Time_zone_info.equal x.tz_info y.tz_info

let to_pattern ({ pick; tz_info = _ } : t) =
  let years =
    match pick with
    | YMDHMSN { year; _ } -> Int_set.add year Int_set.empty
    | _ -> Int_set.empty
  in
  let months =
    match pick with
    | YMDHMSN { month; _ } | MDHMSN { month; _ } ->
        Int_set.add month Int_set.empty
    | _ -> Int_set.empty
  in
  let month_days =
    match pick with
    | YMDHMSN { month_day; _ }
    | MDHMSN { month_day; _ }
    | DHMSN { month_day; _ } ->
        Int_set.add month_day Int_set.empty
    | _ -> Int_set.empty
  in
  let weekdays =
    match pick with
    | WHMSN { weekday; _ } -> Weekday_set.add weekday Weekday_set.empty
    | _ -> Weekday_set.empty
  in
  let hours =
    match pick with
    | YMDHMSN { hour; _ }
    | MDHMSN { hour; _ }
    | WHMSN { hour; _ }
    | DHMSN { hour; _ }
    | HMSN { hour; _ } ->
        Int_set.add hour Int_set.empty
    | _ -> Int_set.empty
  in
  let minutes =
    match pick with
    | YMDHMSN { minute; _ }
    | MDHMSN { minute; _ }
    | WHMSN { minute; _ }
    | DHMSN { minute; _ }
    | HMSN { minute; _ }
    | MSN { minute; _ } ->
        Int_set.add minute Int_set.empty
    | _ -> Int_set.empty
  in
  let seconds =
    match pick with
    | YMDHMSN { second; _ }
    | MDHMSN { second; _ }
    | WHMSN { second; _ }
    | DHMSN { second; _ }
    | HMSN { second; _ }
    | MSN { second; _ }
    | SN { second; _ } ->
        Int_set.add second Int_set.empty
    | _ -> Int_set.empty
  in
  let ns =
    match pick with
    | YMDHMSN { ns; _ }
    | MDHMSN { ns; _ }
    | WHMSN { ns; _ }
    | DHMSN { ns; _ }
    | HMSN { ns; _ }
    | MSN { ns; _ }
    | SN { ns; _ }
    | N ns ->
        Diet.Int.(add (Interval.make ns ns) empty)
  in
  { Pattern.years; months; month_days; weekdays; hours; minutes; seconds; ns }

let to_date_time ~default_tz_info ({ pick; tz_info } : t) : Timedesc.t option =
  let tz_info = match tz_info with None -> default_tz_info | Some x -> x in
  match pick with
  | YMDHMSN { year; month; month_day; hour; minute; second; ns } -> (
      let tz = Timedesc.Time_zone_info.tz tz_info in
      match Timedesc.Time_zone_info.fixed_offset_from_utc tz_info with
      | Some offset_from_utc -> (
          match
            Timedesc.make_unambiguous ~tz ~offset_from_utc ~year ~month
              ~day:month_day ~hour ~minute ~second ~ns ()
          with
          | Ok x -> Some x
          | Error _ -> None)
      | None -> (
          match
            Timedesc.make ~tz ~year ~month ~day:month_day ~hour ~minute ~second
              ~ns ()
          with
          | Ok x -> Some x
          | Error _ -> None))
  | _ -> None
