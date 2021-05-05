open Date_time_components

type timestamp = Span.t

type interval = timestamp * timestamp

let one_ns = Span.make ~ns:1 ()

let timestamp_now () : timestamp = Span.of_float_s @@ Unix.gettimeofday ()

let timestamp_min = Constants.timestamp_min

let timestamp_max = Constants.timestamp_max

module Hms' = struct
  type t = {
    hour : int;
    minute : int;
    second : int;
  }

  type error =
    [ `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    ]

  exception Error_exn of error

  let make ~hour ~minute ~second : (t, error) result =
    if hour < 0 || 24 < hour then Error (`Invalid_hour hour)
    else if minute < 0 || 59 < minute then Error (`Invalid_minute minute)
    else if second < 0 || 60 < second then Error (`Invalid_second second)
    else
      let second = if second = 60 then 59 else second in
      if hour = 24 then
        if minute = 0 && second = 0 then
          Ok { hour = 23; minute = 59; second = 59 }
        else Error (`Invalid_hour hour)
      else Ok { hour; minute; second }

  let make_exn ~hour ~minute ~second =
    match make ~hour ~minute ~second with
    | Ok x -> x
    | Error e -> raise (Error_exn e)

  let to_second_of_day x =
    Span.For_human'.make_exn ~hours:x.hour ~minutes:x.minute ~seconds:x.second
      ()
    |> fun x -> Int64.to_int Span.(x.s)

  let of_second_of_day s =
    let ({ hours; minutes; seconds; _ } : Span.For_human'.view) =
      Span.(make_small ~s () |> For_human'.view)
    in
    match make ~hour:hours ~minute:minutes ~second:seconds with
    | Ok x -> Some x
    | Error _ -> None
end

let utc_tz_info : tz_info = { tz = Time_zone.utc; offset = Some Span.zero }

let dummy_tz_info = utc_tz_info

module type Dt_base = sig
  type t

  type error

  val to_timestamp_pretend_utc : t -> timestamp option

  val get_tz_info : t -> tz_info

  val set_tz_info : t -> tz_info -> t

  val get_second : t -> int

  val set_second : t -> int -> t

  val get_ns : t -> int

  val set_ns : t -> int -> t

  val err_does_not_exist : error

  val err_invalid_tz_info : string option -> Span.t -> error
end

type 'a local_result =
  [ `Single of 'a
  | `Ambiguous of 'a * 'a
  ]

let min_of_local_result (r : 'a local_result) : 'a =
  match r with `Single x | `Ambiguous (x, _) -> x

let max_of_local_result (r : 'a local_result) : 'a =
  match r with `Single x | `Ambiguous (_, x) -> x

module Dt_derive (B : Dt_base) : sig
  type t

  type error

  val of_partial_dt : tz:Time_zone.t -> t -> (t, error) result

  val of_partial_dt_unambiguous : tz:Time_zone.t option -> tz_offset:Span.t -> t -> (t, error) result

  val to_timestamp : t -> timestamp local_result

  val to_timestamp_float : t -> float local_result

  val to_timestamp_single : t -> timestamp

  val to_timestamp_float_single : t -> float
end with type t := B.t and type error := B.error = struct
  include B

  let adjust_ns_for_leap_second ~is_leap_second (dt : t) : t =
    if is_leap_second then set_ns dt (get_ns dt + Span.ns_count_in_s) else dt

  let to_timestamp_precise_unsafe (x : t) : timestamp Time_zone.local_result =
    let open Span in
    match to_timestamp_pretend_utc x with
    | None -> `None
    | Some timestamp_local -> (
        match get_tz_info x with
        | { tz = _; offset = Some offset } -> `Single (timestamp_local - offset)
        | { tz; offset = None } -> (
            match Time_zone.lookup_timestamp_local tz timestamp_local.s with
            | `None -> `None
            | `Single e -> `Single (timestamp_local - make_small ~s:e.offset ())
            | `Ambiguous (e1, e2) ->
              let x1 = timestamp_local - make_small ~s:e1.offset () in
              let x2 = timestamp_local - make_small ~s:e2.offset () in
              `Ambiguous (min x1 x2, max x1 x2)))

  let of_partial_dt ~tz (dt : t) =
    let is_leap_second = get_second dt = 60 in
    let dt = if is_leap_second then set_second dt 59 else dt in
    match to_timestamp_pretend_utc dt with
    | None -> Error err_does_not_exist
    | Some timestamp_local -> (
        match Time_zone.lookup_timestamp_local tz timestamp_local.s with
        | `None -> Error err_does_not_exist
        | `Single e ->
          Ok
            (set_tz_info dt
               { tz; offset = Some (Span.make_small ~s:e.offset ()) }
            )
        | `Ambiguous _ -> Ok
                            (set_tz_info dt
                               { tz; offset = None }
                            )
      )
        |> CCResult.map (adjust_ns_for_leap_second ~is_leap_second)

  let of_partial_dt_unambiguous ~tz ~tz_offset (dt : t) =
    let make_invalid_tz_info_error ?tz ~tz_offset () =
      Error (err_invalid_tz_info (CCOpt.map Time_zone.name tz) tz_offset)
    in
    let is_leap_second = get_second dt = 60 in
    let dt = if is_leap_second then set_second dt 59 else dt in
    (
      match make_tz_info ?tz ~tz_offset () with
      | Error `Missing_both_tz_and_tz_offset -> failwith "Unexpected case"
      | Error (`Invalid_offset _) | Error (`Unrecorded_offset _) ->
        make_invalid_tz_info_error ?tz ~tz_offset ()
      | Ok ({ tz = tz'; offset = _ } as tz_info) -> (
          match
            to_timestamp_pretend_utc dt
          with
          | None -> Error err_does_not_exist
          | Some { s = timestamp_local; ns = _ } -> (
              let tz_offset_s = Int64.to_int tz_offset.s in
              match
                Time_zone.lookup_timestamp_local tz' timestamp_local
              with
              | `None -> Error err_does_not_exist
              | `Single e ->
                if e.offset = tz_offset_s then Ok tz_info
                else make_invalid_tz_info_error ?tz ~tz_offset ()
              | `Ambiguous (e1, e2) ->
                if e1.offset = tz_offset_s || e2.offset = tz_offset_s then
                  Ok tz_info
                else make_invalid_tz_info_error ?tz ~tz_offset ()))
    )
    |> CCResult.map (set_tz_info dt)
    |> CCResult.map (adjust_ns_for_leap_second ~is_leap_second)

  let to_timestamp x : timestamp local_result =
    match to_timestamp_precise_unsafe x with
    | `None -> failwith "Unexpected case"
    | `Single x -> `Single x
    | `Ambiguous (x, y) -> `Ambiguous (x, y)

  let to_timestamp_float x : float local_result =
    match to_timestamp_precise_unsafe x with
    | `None -> failwith "Unexpected case"
    | `Single x -> `Single (Span.to_float_s x)
    | `Ambiguous (x, y) -> `Ambiguous (Span.to_float_s x, Span.to_float_s y)

  let to_timestamp_single (x : t) : timestamp =
    match to_timestamp x with
    | `Single x -> x
    | `Ambiguous _ ->
      invalid_arg "to_timestamp_single: date time maps to two timestamps"

  let to_timestamp_float_single (x : t) : float =
    match to_timestamp x with
    | `Single x -> Span.to_float_s x
    | `Ambiguous _ ->
      invalid_arg
        "to_timestamp_float_single: date time maps to two timestamps"
end

module Odt' = struct
  type t = {
    year : int;
    day : int;
    hour : int;
    minute : int;
    second : int;
    ns : int;
    tz_info : tz_info;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_day of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_frac of float
    | `Invalid_ns of int
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_exn of error

  let check_args_and_normalize_ns ~year ~day ~hour ~minute ~second ~ns ~frac :
    (int, error) result =
    if year < Constants.min_year || Constants.max_year < year then
      Error (`Invalid_year year)
    else if day < 1 || 366 < day then Error (`Invalid_day day)
    else if hour < 0 || 23 < hour then Error (`Invalid_hour hour)
    else if minute < 0 || 59 < minute then Error (`Invalid_minute minute)
    else if second < 0 || 60 < second then Error (`Invalid_second second)
    else if frac < 0. then Error (`Invalid_frac frac)
    else if ns < 0 then Error (`Invalid_ns ns)
    else
      let ns = ns + int_of_float (frac *. Span.ns_count_in_s_float) in
      if ns >= Span.ns_count_in_s then Error (`Invalid_ns ns) else Ok ns

  let to_timestamp_pretend_utc (x : t) : Span.t option =
    if not (is_leap_year ~year:x.year) && x.day > 365 then
      None
    else
      Some
        Span.(
          let month, day = md_of_ydoy ~year:x.year ~day_of_year:x.day in
          For_human'.make_exn ~days:(jd_of_ymd ~year:x.year ~month ~day) ()
          + For_human'.make_exn ~days:Stdlib.(x.day - 1) ~hours:x.hour ~minutes:x.minute
            ~seconds:x.second ~ns:x.ns ()
        )

  include Dt_derive (struct
      type nonrec t = t

      type nonrec error = error

      let to_timestamp_pretend_utc = to_timestamp_pretend_utc

      let get_tz_info (x : t) = x.tz_info

      let set_tz_info x tz_info =
        { x with tz_info }

      let get_second x = x.second

      let set_second x second =
        { x with second }

      let get_ns x = x.ns

      let set_ns x ns =
        { x with ns }

      let err_does_not_exist = `Does_not_exist

      let err_invalid_tz_info tz_name tz_offset =
        `Invalid_tz_info (tz_name, tz_offset)
    end)

  let make ?(tz = Time_zone_utils.get_local_tz_for_arg ()) ?(ns = 0)
      ?(frac = 0.) ~year ~day ~hour ~minute ~second () =
    match
      check_args_and_normalize_ns ~year ~day ~hour ~minute ~second ~ns
        ~frac
    with
    | Error e -> Error e
    | Ok ns ->
      of_partial_dt ~tz
        {
          year;
          day;
          hour;
          minute;
          second;
          ns;
          tz_info = dummy_tz_info;
        }

  let weekday (x : t) =
    let month, day = md_of_ydoy ~year:x.year ~day_of_year:x.day in
    CCOpt.get_exn @@ weekday_of_ymd ~year:x.year ~month ~day

  let of_timestamp_local (x : Span.t) =
    let x = Span.For_human'.view x in
    let year, month, day = ymd_of_jd (jd_of_epoch + x.days) in
    let doy = doy_of_ymd ~year ~month ~day in
    let hour, minute, second, ns =
      match x.sign with
      | `Pos -> (x.hours, x.minutes, x.seconds, x.ns)
      | `Neg -> (x.hours, x.minutes, x.seconds, x.ns)
    in
    { year; day; hour }

  let of_timestamp ?(tz_of_date_time = Time_zone_utils.get_local_tz_for_arg ())
      (x : timestamp) : t option =
    if not Span.(timestamp_min <= x && x <= timestamp_max) then None
    else
      match Time_zone.lookup_timestamp_utc tz_of_date_time x.s with
      | None -> None
      | Some entry -> (
          let timestamp_local =
            Span.(x - make_small ~s:entry.offset ())
          in
          match Ptime_utils.ptime_of_timestamp x with
          | None -> None
          | Some x ->
            x
            |> Ptime.to_date_time ~tz_offset_s:entry.offset
            |> of_ptime_date_time_pretend_utc
            |> fun t ->
            Some
              {
                t with
                ns;
                tz_info =
                  {
                    tz = tz_of_date_time;
                    offset = Some (Span.make_small ~s:entry.offset ());
                  };
              }
        )

end

module Dt' = struct
  type t = {
    year : int;
    month : int;
    day : int;
    hour : int;
    minute : int;
    second : int;
    ns : int;
    tz_info : tz_info;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_month of int
    | `Invalid_day of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_frac of float
    | `Invalid_ns of int
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_exn of error

  let string_of_error (e : error) =
    match e with
    | `Does_not_exist -> "Does not exist"
    | `Invalid_year x -> Printf.sprintf "Invalid year: %d" x
    | `Invalid_month x -> Printf.sprintf "Invalid month: %d" x
    | `Invalid_day x -> Printf.sprintf "Invalid day: %d" x
    | `Invalid_hour x -> Printf.sprintf "Invalid hour: %d" x
    | `Invalid_minute x -> Printf.sprintf "Invalid minute: %d" x
    | `Invalid_second x -> Printf.sprintf "Invalid second: %d" x
    | `Invalid_frac x -> Printf.sprintf "Invalid frac: %f" x
    | `Invalid_ns x -> Printf.sprintf "Invalid ns: %d" x
    | `Invalid_tz_info (tz, offset) ->
      let offset = Span.For_human'.view offset in
      Printf.sprintf "Invalid tz info: %s, %c%d:%d"
        (match tz with None -> "None" | Some tz -> tz)
        Span.For_human'.(match offset.sign with `Pos -> '+' | `Neg -> '-')
        Span.For_human'.(offset.hours)
        Span.For_human'.(offset.minutes)

  let of_odt (x : Odt'.t) : t =
    let (month, day) =
      to_md x
    in
    {
      year = x.year;
      month;
      day;
      hour = x.hour;
      minute = x.minute;
      second = x.second;
      ns = x.ns;
      tz_info = x.tz_info;
    }

  let to_odt (x : t) : Odt'.t =
    let offset =
      day_offset_from_start_of_year_lookup ~year:x.year ~month:x.month
    in
    {
      year = x.year;
      day = offset + x.day;
      hour = x.hour;
      minute = x.minute;
      second = x.second;
      ns = x.ns;
      tz_info = x.tz_info;
    }

  let of_timestamp ?(tz_of_date_time = Time_zone_utils.get_local_tz_for_arg ())
      ({ s; ns } as x : timestamp) : t option =
    if not Span.(timestamp_min <= x && x <= timestamp_max) then None
    else
      match Time_zone.lookup_timestamp_utc tz_of_date_time s with
      | None -> None
      | Some entry -> (
          match Ptime_utils.ptime_of_timestamp x with
          | None -> None
          | Some x ->
            x
            |> Ptime.to_date_time ~tz_offset_s:entry.offset
            |> of_ptime_date_time_pretend_utc
            |> fun t ->
            Some
              {
                t with
                ns;
                tz_info =
                  {
                    tz = tz_of_date_time;
                    offset = Some (Span.make_small ~s:entry.offset ());
                  };
              })

  let of_timestamp_exn ?tz_of_date_time x =
    match of_timestamp ?tz_of_date_time x with
    | None -> invalid_arg "of_timestamp_exn"
    | Some x -> x

  let of_timestamp_float ?tz_of_date_time (x : float) : t option =
    of_timestamp ?tz_of_date_time @@ Span.of_float_s x

  let of_timestamp_float_exn ?tz_of_date_time x =
    match of_timestamp_float ?tz_of_date_time x with
    | None -> invalid_arg "of_timestamp_exn"
    | Some x -> x

  let is_leap_second (dt : t) = dt.ns >= Span.ns_count_in_s

  let check_args_and_normalize_ns ~year ~month ~day ~hour ~minute ~second ~ns
      ~frac : (int, error) result =
    if year < Constants.min_year || Constants.max_year < year then
      Error (`Invalid_year year)
    else if month < 1 || 12 < month then Error (`Invalid_month month)
    else if day < 1 || 31 < day then Error (`Invalid_day day)
    else if hour < 0 || 23 < hour then Error (`Invalid_hour hour)
    else if minute < 0 || 59 < minute then Error (`Invalid_minute minute)
    else if second < 0 || 60 < second then Error (`Invalid_second second)
    else if frac < 0. then Error (`Invalid_frac frac)
    else if ns < 0 then Error (`Invalid_ns ns)
    else
      let ns = ns + int_of_float (frac *. Span.ns_count_in_s_float) in
      if ns >= Span.ns_count_in_s then Error (`Invalid_ns ns) else Ok ns

  let make ?(tz = Time_zone_utils.get_local_tz_for_arg ()) ?(ns = 0)
      ?(frac = 0.) ~year ~month ~day ~hour ~minute ~second () =
    match
      check_args_and_normalize_ns ~year ~month ~day ~hour ~minute ~second ~ns
        ~frac
    with
    | Error e -> Error e
    | Ok ns ->
      of_partial_dt
        ~tz
        {
          year;
          month;
          day;
          hour;
          minute;
          second;
          ns;
          tz_info = dummy_tz_info;
        }

  let make_exn ?tz ?ns ?frac ~year ~month ~day ~hour ~minute ~second () =
    match make ?tz ~year ~month ~day ~hour ~minute ~second ?ns ?frac () with
    | Ok x -> x
    | Error e -> raise (Error_exn e)

  let make_unambiguous ?tz ?(ns = 0) ?(frac = 0.) ~year ~month ~day ~hour
      ~minute ~second ~tz_offset () =
    match
      check_args_and_normalize_ns ~year ~month ~day ~hour ~minute ~second ~ns
        ~frac
    with
    | Error e -> Error e
    | Ok ns ->
      of_partial_dt_unambiguous
        ~tz ~tz_offset
        {
          year;
          month;
          day;
          hour;
          minute;
          second;
          ns;
          tz_info = dummy_tz_info;
        }

  let make_unambiguous_exn ?tz ?ns ?frac ~year ~month ~day ~hour ~minute ~second
      ~tz_offset () =
    match
      make_unambiguous ?tz ~year ~month ~day ~hour ~minute ~second ?ns ?frac
        ~tz_offset ()
    with
    | Ok x -> x
    | Error e -> raise (Error_exn e)

  let min_val =
    CCOpt.get_exn @@ of_timestamp ~tz_of_date_time:Time_zone.utc timestamp_min

  let max_val =
    CCOpt.get_exn @@ of_timestamp ~tz_of_date_time:Time_zone.utc timestamp_max

  let now ?tz_of_date_time () : t =
    timestamp_now () |> of_timestamp ?tz_of_date_time |> CCOpt.get_exn

  let weekday (x : t) : weekday =
    CCOpt.get_exn @@ weekday_of_month_day ~year:x.year ~month:x.month ~day:x.day

  let equal (x : t) (y : t) : bool =
    x.year = y.year
    && x.month = y.month
    && x.day = y.day
    && x.hour = y.hour
    && x.minute = y.minute
    && x.second = y.second
    && x.ns = y.ns
    && equal_tz_info x.tz_info y.tz_info

  let set_to_first_ns (x : t) : t = { x with ns = 0 }

  let set_to_last_ns (x : t) : t = { x with ns = Span.ns_count_in_s - 1 }

  let set_to_first_sec_ns (x : t) : t = { x with second = 0 } |> set_to_first_ns

  let set_to_last_sec_ns (x : t) : t = { x with second = 59 } |> set_to_last_ns

  let set_to_first_min_sec_ns (x : t) : t =
    { x with minute = 0 } |> set_to_first_sec_ns

  let set_to_last_min_sec_ns (x : t) : t =
    { x with minute = 59 } |> set_to_last_sec_ns

  let set_to_first_hour_min_sec_ns (x : t) : t =
    { x with hour = 0 } |> set_to_first_min_sec_ns

  let set_to_last_hour_min_sec_ns (x : t) : t =
    { x with hour = 23 } |> set_to_last_min_sec_ns

  let set_to_first_day_hour_min_sec_ns (x : t) : t =
    { x with day = 1 } |> set_to_first_hour_min_sec_ns

  let set_to_last_day_hour_min_sec_ns (x : t) : t =
    { x with day = day_count_of_month ~year:x.year ~month:x.month }
    |> set_to_last_hour_min_sec_ns

  let set_to_first_month_day_hour_min_sec_ns (x : t) : t =
    { x with month = 1 } |> set_to_first_day_hour_min_sec_ns

  let set_to_last_month_day_hour_min_sec_ns (x : t) : t =
    { x with month = 12 } |> set_to_last_day_hour_min_sec_ns
end

module Week_date_time' = struct
  type t = {
    year : int;
    week : int;
    weekday : weekday;
    hour : int;
    minute : int;
    second : int;
    ns : int;
    tz_info : tz_info;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_week of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_frac of float
    | `Invalid_ns of int
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_exn of error

  let check_args_and_normalize_ns ~year ~week ~hour ~minute ~second ~ns ~frac :
    (int, error) result =
    if year < Constants.min_year || Constants.max_year < year then
      Error (`Invalid_year year)
    else if week < 1 || 53 < week then Error (`Invalid_week week)
    else if hour < 0 || 23 < hour then Error (`Invalid_hour hour)
    else if minute < 0 || 59 < minute then Error (`Invalid_minute minute)
    else if second < 0 || 60 < second then Error (`Invalid_second second)
    else if frac < 0. then Error (`Invalid_frac frac)
    else if ns < 0 then Error (`Invalid_ns ns)
    else
      let ns = ns + int_of_float (frac *. Span.ns_count_in_s_float) in
      if ns >= Span.ns_count_in_s then Error (`Invalid_ns ns) else Ok ns

  let date_time_local_start_of_iso_week_year ~iso_week_year : Date_time'.t =
    (* we use the fact that
       - Jan 4th is always in week 1 of the year
       - and week starts on Monday

       to find the start date of week 1
    *)
    let tz = Time_zone.utc in
    let year = iso_week_year in
    let jan_4_of_year =
      Date_time'.make_exn ~tz ~year ~month:1 ~day:4 ~hour:0
        ~minute:0 ~second:0 ()
    in
    (match Date_time'.weekday jan_4_of_year with
     | `Mon -> jan_4_of_year
     | `Tue ->
         (Date_time'.make_exn ~tz ~year ~month:1 ~day:3 ~hour:0
            ~minute:0 ~second:0 ())
     | `Wed ->
         (Date_time'.make_exn ~tz ~year ~month:1 ~day:2 ~hour:0
            ~minute:0 ~second:0 ())
     | `Thu ->
         (Date_time'.make_exn ~tz ~year ~month:1 ~day:1 ~hour:0
            ~minute:0 ~second:0 ())
     | `Fri -> (
           Date_time'.make_exn ~tz ~year:(pred year) ~month:12 ~day:31
             ~hour:0 ~minute:0 ~second:0 ()
)
     | `Sat -> (
           Date_time'.make_exn ~tz ~year:(pred year) ~month:12 ~day:30
             ~hour:0 ~minute:0 ~second:0 ()
         )
     | `Sun -> (
           Date_time'.make_exn ~tz ~year:(pred year) ~month:12 ~day:29
             ~hour:0 ~minute:0 ~second:0 ()
         ))

  let timestamp_local_start_of_iso_week_year ~iso_week_year =
    Date_time'.to_timestamp_single
      (date_time_local_start_of_iso_week_year ~iso_week_year)

  let timestamp_local_start_and_week_count_of_iso_week_year ~iso_week_year :
    (timestamp * int) =
    let start = timestamp_local_start_of_iso_week_year ~iso_week_year in
    let end_exc = timestamp_local_start_of_iso_week_year ~iso_week_year:(succ iso_week_year) in
    let d = Span.(end_exc - start |> For_human'.view) in
    let week_count = d.days / 7 in
    assert (week_count >= 1);
    (start, week_count)

  let week_count_of_iso_week_year ~iso_week_year =
    snd
      (
        timestamp_local_start_and_week_count_of_iso_week_year ~iso_week_year
      )

  let day_index_of_weekday (weekday : weekday) =
    match weekday with
    | `Mon -> 0
    | `Tue -> 1
    | `Wed -> 2
    | `Thu -> 3
    | `Fri -> 4
    | `Sat -> 5
    | `Sun -> 6

  let of_ordinal_date_time (x : Ordinal_date_time'.t) : t =
    let weekday =
      Ordinal_date_time'.weekday x
    in
    let week_of_year =
      (10 + x.day - (day_index_of_weekday weekday + 1)) / 7
    in
    assert (week_of_year >= 0);
    assert (week_of_year <= 53);
    let year, week =
      if week_of_year = 0 then
        if x.year = 0 then
          (-1, 52)
        else
          (pred x.year, week_count_of_iso_week_year ~iso_week_year:(pred x.year))
      else
      if week_of_year = 53 && week_count_of_iso_week_year ~iso_week_year:x.year < 53 then
        (succ x.year, 1)
      else
        (x.year, week_of_year)
    in
    {
      year;
      week;
      weekday;
      hour = x.hour;
      minute = x.minute;
      second = x.second;
      ns = x.ns;
      tz_info = x.tz_info;
    }

  let to_ordinal_date_time (x : t) : Ordinal_date_time'.t =
    let weekday_int = day_index_of_weekday x.weekday + 1 in
    let jan_4_weekday_int = day_index_of_weekday (CCOpt.get_exn @@ weekday_of_month_day ~year:x.year ~month:1 ~day:4) + 1 in
    let day_of_year = x.week * 7 + weekday_int - (jan_4_weekday_int + 3) in
    let day_count_of_prev_year = day_count_of_year ~year:(pred x.year) in
    let day_count_of_cur_year = day_count_of_year ~year:x.year in
    let year, day_of_year =
      if day_of_year < 0 then
        (pred x.year, day_count_of_prev_year - day_of_year)
      else
        if day_of_year > day_count_of_cur_year then
          (succ x.year, day_of_year - day_count_of_cur_year)
        else
          (x.year, day_count_of_cur_year)
    in
    {
      year;
      day = day_of_year;
      hour = x.hour;
      minute = x.minute;
      second = x.second;
      ns = x.ns;
      tz_info = x.tz_info;
    }

  let to_timestamp_pretend_utc (x : t) : Span.t option =
    let (start, week_count) = timestamp_local_start_and_week_count_of_iso_week_year ~iso_week_year:x.year in
    if x.week > week_count then
      None
    else
      let offset =
        Span.For_human'.(
          make_exn
            ~days:(((x.week - 1) * 7) + day_index_of_weekday x.weekday)
            ())
      in
      Some Span.(start + offset)

  include Dt_derive (struct
      type nonrec t = t

      type nonrec error = error

      let to_timestamp_pretend_utc = to_timestamp_pretend_utc

      let get_tz_info (x : t) = x.tz_info

      let set_tz_info x tz_info =
        { x with tz_info }

      let get_second x = x.second

      let set_second x second =
        { x with second }

      let get_ns x = x.ns

      let set_ns x ns =
        { x with ns }

      let err_does_not_exist = `Does_not_exist

      let err_invalid_tz_info tz_name tz_offset =
        `Invalid_tz_info (tz_name, tz_offset)
    end)

  let make ?(tz = Time_zone_utils.get_local_tz_for_arg ()) ?(ns = 0)
      ?(frac = 0.) ~year ~week ~weekday ~hour ~minute ~second () =
    match
      check_args_and_normalize_ns ~year ~week ~hour ~minute ~second ~ns ~frac
    with
    | Error e -> Error e
    | Ok ns ->
        of_partial_dt ~tz
          {
            year;
            week;
            weekday;
            hour;
            minute;
            second;
            ns;
            tz_info = Date_time'.dummy_tz_info;
          }

  let make_exn ?tz ?ns ?frac ~year ~week ~weekday ~hour ~minute ~second () =
    match make ?tz ?ns ?frac ~year ~week ~weekday ~hour ~minute ~second () with
    | Ok x -> x
    | Error e -> raise (Error_exn e)
end

type t = {
  iso_year : int;
}

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
