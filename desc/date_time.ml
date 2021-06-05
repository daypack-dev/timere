open Date_time_utils

type timestamp = Span.t

type interval = timestamp * timestamp

let one_ns = Span.make ~ns:1 ()

let timestamp_now () : timestamp = Span.of_float_s @@ Unix.gettimeofday ()

let timestamp_min = Constants.timestamp_min

let timestamp_max = Constants.timestamp_max

let dummy_tz = Time_zone.utc

let dummy_offset_from_utc = `Single Span.zero

type 'a local_result =
  [ `Single of 'a
  | `Ambiguous of 'a * 'a
  ]

let equal_local_result ~eq (x : 'a local_result) (y : 'a local_result) =
  match (x, y) with
  | `Single x, `Single y -> eq x y
  | `Ambiguous (x1, x2), `Ambiguous (y1, y2) -> eq x1 y1 && eq x2 y2
  | _, _ -> false

let min_of_local_result (r : 'a local_result) : 'a =
  match r with `Single x | `Ambiguous (x, _) -> x

let max_of_local_result (r : 'a local_result) : 'a =
  match r with `Single x | `Ambiguous (_, x) -> x

type t = {
  date : Date.t;
  time : Time.t;
  tz : Time_zone.t;
  offset_from_utc : Span.t local_result;
}

let jd_span_of_unix_epoch = Span.For_human'.make_exn ~days:jd_of_unix_epoch ()

let timestamp_local_of_date_and_time (date : Date.t) (time : Time.t) : Span.t =
  Span.(
    For_human'.make_exn ~days:date.jd ()
    - jd_span_of_unix_epoch
    + Time.to_span time)

let to_timestamp_local (x : t) : Span.t =
  (* we obtain the local timestamp by pretending we are in the UTC time zone,
     i.e. ignoring the time zone information
  *)
  timestamp_local_of_date_and_time x.date x.time

let to_timestamp_precise_unsafe (x : t) : timestamp Time_zone.local_result =
  let open Span in
  let timestamp_local = to_timestamp_local x in
  match x.offset_from_utc with
  | `Single offset -> `Single (timestamp_local - offset)
  | `Ambiguous (offset1, offset2) ->
      let x1 = timestamp_local - offset1 in
      let x2 = timestamp_local - offset2 in
      `Ambiguous (min x1 x2, max x1 x2)

let to_timestamp x : timestamp local_result =
  match to_timestamp_precise_unsafe x with
  | `None -> failwith "Unexpected case"
  | `Single x -> `Single x
  | `Ambiguous (x, y) -> `Ambiguous (x, y)

let to_timestamp_float_s x : float local_result =
  match to_timestamp_precise_unsafe x with
  | `None -> failwith "Unexpected case"
  | `Single x -> `Single (Span.to_float_s x)
  | `Ambiguous (x, y) -> `Ambiguous (Span.to_float_s x, Span.to_float_s y)

let to_timestamp_single (x : t) : timestamp =
  match to_timestamp x with
  | `Single x -> x
  | `Ambiguous _ ->
      invalid_arg "to_timestamp_single: date time maps to two timestamps"

let to_timestamp_float_s_single (x : t) : float =
  match to_timestamp x with
  | `Single x -> Span.to_float_s x
  | `Ambiguous _ ->
      invalid_arg
        "to_timestamp_float_s_single: date time maps to two timestamps"

let of_timestamp_local (x : Span.t) =
  let x = Span.(jd_span_of_unix_epoch + x) in
  let v = Span.For_human'.view x in
  let year, month, day = ymd_of_jd v.days in
  let hour, minute, second, ns = (v.hours, v.minutes, v.seconds, v.ns) in
  let date = Date.Ymd_date.make_exn ~year ~month ~day in
  let time = Time.make_exn ~hour ~minute ~second ~ns () in
  { date; time; tz = Time_zone.utc; offset_from_utc = `Single Span.zero }

let of_timestamp ?(tz_of_date_time = Time_zone_utils.get_local_tz_for_arg ())
    (x : timestamp) : t option =
  if not Span.(timestamp_min <= x && x <= timestamp_max) then None
  else
    match Time_zone.lookup_timestamp_utc tz_of_date_time (Span.get_s x) with
    | None -> None
    | Some entry ->
        let timestamp_local = Span.(x + make_small ~s:entry.offset ()) in
        Some
          {
            (of_timestamp_local timestamp_local) with
            tz = tz_of_date_time;
            offset_from_utc = `Single (Span.make_small ~s:entry.offset ());
          }

let of_timestamp_exn ?tz_of_date_time x =
  match of_timestamp ?tz_of_date_time x with
  | None -> invalid_arg "of_timestamp_exn"
  | Some x -> x

let of_timestamp_float_s ?tz_of_date_time (x : float) : t option =
  of_timestamp ?tz_of_date_time @@ Span.of_float_s x

let of_timestamp_float_s_exn ?tz_of_date_time x =
  match of_timestamp_float_s ?tz_of_date_time x with
  | None -> invalid_arg "of_timestamp_exn"
  | Some x -> x

let equal (x : t) (y : t) =
  Date.equal x.date y.date
  && Time.equal x.time y.time
  && Time_zone.equal x.tz y.tz
  && equal_local_result ~eq:Span.equal x.offset_from_utc y.offset_from_utc

let now ?tz_of_date_time () : t =
  timestamp_now ()
  |> of_timestamp ?tz_of_date_time
  |> CCOpt.get_exn_or "Expected successful date time construction for now"

let min_val =
  CCOpt.get_exn_or "Expected successful date time construction for min_val"
  @@ of_timestamp ~tz_of_date_time:Time_zone.utc timestamp_min

let max_val =
  CCOpt.get_exn_or "Expected successful date time construction for max_val"
  @@ of_timestamp ~tz_of_date_time:Time_zone.utc timestamp_max

let is_leap_second (dt : t) = Time.is_leap_second dt.time

let weekday dt = Date.weekday dt.date

let date dt = dt.date

let ymd_date dt = Date.Ymd_date.view dt.date

let iso_week_date dt = Date.ISO_week_date.view dt.date

let iso_ord_date dt = Date.ISO_ord_date.view dt.date

let year dt = Date.year dt.date

let month dt = Date.month dt.date

let day dt = Date.day dt.date

let iso_week_year dt = Date.iso_week_year dt.date

let iso_week dt = Date.iso_week dt.date

let day_of_year dt = Date.day_of_year dt.date

let time dt = dt.time

let time_view dt = Time.view dt.time

let hour dt = Time.hour dt.time

let minute dt = Time.minute dt.time

let second dt = Time.second dt.time

let ns dt = Time.ns dt.time

let tz (dt : t) = dt.tz

let offset_from_utc (dt : t) = dt.offset_from_utc

module Zoneless' = struct
  type zoneless = {
    date : Date.t;
    time : Time.t;
  }

  type error_when_zoned =
    [ `Does_not_exist
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_when_zoned_exn of error_when_zoned

  let make date time = { date; time }

  let equal (x : zoneless) (y : zoneless) =
    Date.equal x.date y.date && Time.equal x.time y.time

  let date (x : zoneless) = x.date

  let time (x : zoneless) = x.time

  let to_timestamp_local (x : zoneless) =
    timestamp_local_of_date_and_time x.date x.time

  let to_zoned ?(tz = Time_zone_utils.get_local_tz_for_arg ())
      ({ date; time } as zl : zoneless) : (t, error_when_zoned) result =
    let timestamp_local = to_timestamp_local zl in
    match Time_zone.lookup_timestamp_local tz (Span.get_s timestamp_local) with
    | `None -> Error `Does_not_exist
    | `Single e ->
        Ok
          {
            date;
            time;
            tz;
            offset_from_utc = `Single (Span.make_small ~s:e.offset ());
          }
    | `Ambiguous (e1, e2) ->
        let x1 = Span.make_small ~s:e1.offset () in
        let x2 = Span.make_small ~s:e2.offset () in
        Ok { date; time; tz; offset_from_utc = `Ambiguous (x1, x2) }

  let to_zoned_exn ?tz x =
    match to_zoned ?tz x with
    | Ok x -> x
    | Error e -> raise (Error_when_zoned_exn e)

  let to_zoned_unambiguous ?tz ~offset_from_utc
      ({ date; time } as zl : zoneless) : (t, error_when_zoned) result =
    let make_invalid_tz_info_error ?tz ~offset_from_utc () =
      Error (`Invalid_tz_info (CCOpt.map Time_zone.name tz, offset_from_utc))
    in
    (match
       Time_zone_info.make ?tz ~fixed_offset_from_utc:offset_from_utc ()
     with
    | Error `Missing_both_tz_and_fixed_offset_from_utc ->
        failwith "Unexpected case"
    | Error (`Invalid_offset _) | Error (`Unrecorded_offset _) ->
        make_invalid_tz_info_error ?tz ~offset_from_utc ()
    | Ok ({ tz = tz'; fixed_offset_from_utc = _ } as tz_info) -> (
        let timestamp_local = Span.get_s @@ to_timestamp_local zl in
        let offset_from_utc_s = Int64.to_int @@ Span.get_s offset_from_utc in
        match Time_zone.lookup_timestamp_local tz' timestamp_local with
        | `None -> Error `Does_not_exist
        | `Single e ->
            if e.offset = offset_from_utc_s then Ok tz_info
            else make_invalid_tz_info_error ?tz ~offset_from_utc ()
        | `Ambiguous (e1, e2) ->
            if e1.offset = offset_from_utc_s || e2.offset = offset_from_utc_s
            then Ok tz_info
            else make_invalid_tz_info_error ?tz ~offset_from_utc ()))
    |> CCResult.map (fun ({ tz; fixed_offset_from_utc } : Time_zone_info.t) ->
           {
             date;
             time;
             tz;
             offset_from_utc =
               `Single
                 (CCOpt.get_exn_or
                    "Expected fixed_offset_from_utc in tz_info to be Some _"
                    fixed_offset_from_utc);
           })

  let to_zoned_unambiguous_exn ?tz ~offset_from_utc zl =
    match to_zoned_unambiguous ?tz ~offset_from_utc zl with
    | Ok x -> x
    | Error e -> raise (Error_when_zoned_exn e)

  let of_zoned (x : t) : zoneless = { date = x.date; time = x.time }
end

let compare_chrono_min (x : t) (y : t) : int =
  let x = min_of_local_result @@ to_timestamp x in
  let y = min_of_local_result @@ to_timestamp y in
  Span.compare x y

let compare_chrono_max (x : t) (y : t) : int =
  let x = max_of_local_result @@ to_timestamp x in
  let y = max_of_local_result @@ to_timestamp y in
  Span.compare x y

let compare_struct (x : t) (y : t) : int =
  let cmp_res = String.compare (Time_zone.name x.tz) (Time_zone.name y.tz) in
  let tz_lt = cmp_res < 0 in
  let tz_eq = cmp_res = 0 in
  let x_timestamp_local = Zoneless'.(to_timestamp_local @@ of_zoned x) in
  let y_timestamp_local = Zoneless'.(to_timestamp_local @@ of_zoned y) in
  let lt = tz_lt || (tz_eq && Span.lt x_timestamp_local y_timestamp_local) in
  let eq = tz_eq && Span.equal x_timestamp_local y_timestamp_local in
  if lt then -1 else if eq then 0 else 1

module ISO_ord_date_time = struct
  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_day_of_year of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_s_frac of float
    | `Invalid_ns of int
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_exn of error

  let make ?tz ?ns ?s_frac ~year ~day_of_year ~hour ~minute ~second () :
      (t, error) result =
    match Date.ISO_ord_date.make ~year ~day_of_year with
    | Error e -> Error (e :> error)
    | Ok date -> (
        match Time.make ~hour ~minute ~second ?ns ?s_frac () with
        | Error e -> Error (e :> error)
        | Ok time -> (
            match Zoneless'.to_zoned ?tz { date; time } with
            | Error e -> Error (e :> error)
            | Ok dt -> Ok dt))

  let make_exn ?tz ?ns ?s_frac ~year ~day_of_year ~hour ~minute ~second () =
    match make ?tz ~year ~day_of_year ~hour ~minute ~second ?ns ?s_frac () with
    | Ok x -> x
    | Error e -> raise (Error_exn e)

  let make_unambiguous ?tz ?ns ?s_frac ~year ~day_of_year ~hour ~minute ~second
      ~offset_from_utc () =
    match Date.ISO_ord_date.make ~year ~day_of_year with
    | Error e -> Error (e :> error)
    | Ok date -> (
        match Time.make ~hour ~minute ~second ?ns ?s_frac () with
        | Error e -> Error (e :> error)
        | Ok time -> (
            match
              Zoneless'.to_zoned_unambiguous ?tz ~offset_from_utc { date; time }
            with
            | Ok dt -> Ok dt
            | Error e -> Error (e :> error)))

  let make_unambiguous_exn ?tz ?ns ?s_frac ~year ~day_of_year ~hour ~minute
      ~second ~offset_from_utc () =
    match
      make_unambiguous ?tz ?ns ?s_frac ~year ~day_of_year ~hour ~minute ~second
        ~offset_from_utc ()
    with
    | Ok x -> x
    | Error e -> raise (Error_exn e)
end

module Ymd_date_time = struct
  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_month of int
    | `Invalid_day of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_s_frac of float
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
    | `Invalid_s_frac x -> Printf.sprintf "Invalid frac: %f" x
    | `Invalid_ns x -> Printf.sprintf "Invalid ns: %d" x
    | `Invalid_tz_info (tz, offset) ->
        let offset = Span.For_human'.view offset in
        Printf.sprintf "Invalid tz info: %s, %c%d:%d"
          (match tz with None -> "None" | Some tz -> tz)
          Span.For_human'.(match offset.sign with `Pos -> '+' | `Neg -> '-')
          Span.For_human'.(offset.hours)
          Span.For_human'.(offset.minutes)

  let make ?tz ?ns ?s_frac ~year ~month ~day ~hour ~minute ~second () :
      (t, error) result =
    match Date.Ymd_date.make ~year ~month ~day with
    | Error e -> Error (e :> error)
    | Ok date -> (
        match Time.make ~hour ~minute ~second ?ns ?s_frac () with
        | Error e -> Error (e :> error)
        | Ok time -> (
            match Zoneless'.to_zoned ?tz { date; time } with
            | Error e -> Error (e :> error)
            | Ok dt -> Ok dt))

  let make_exn ?tz ?ns ?s_frac ~year ~month ~day ~hour ~minute ~second () =
    match make ?tz ~year ~month ~day ~hour ~minute ~second ?ns ?s_frac () with
    | Ok x -> x
    | Error e -> raise (Error_exn e)

  let make_unambiguous ?tz ?ns ?s_frac ~year ~month ~day ~hour ~minute ~second
      ~offset_from_utc () =
    match Date.Ymd_date.make ~year ~month ~day with
    | Error e -> Error (e :> error)
    | Ok date -> (
        match Time.make ~hour ~minute ~second ?ns ?s_frac () with
        | Error e -> Error (e :> error)
        | Ok time -> (
            match
              Zoneless'.to_zoned_unambiguous ?tz ~offset_from_utc { date; time }
            with
            | Ok dt -> Ok dt
            | Error e -> Error (e :> error)))

  let make_unambiguous_exn ?tz ?ns ?s_frac ~year ~month ~day ~hour ~minute
      ~second ~offset_from_utc () =
    match
      make_unambiguous ?tz ~year ~month ~day ~hour ~minute ~second ?ns ?s_frac
        ~offset_from_utc ()
    with
    | Ok x -> x
    | Error e -> raise (Error_exn e)
end

module ISO_week_date_time = struct
  type error =
    [ `Does_not_exist
    | `Invalid_iso_week_year of int
    | `Invalid_iso_week of int
    | `Invalid_hour of int
    | `Invalid_minute of int
    | `Invalid_second of int
    | `Invalid_s_frac of float
    | `Invalid_ns of int
    | `Invalid_tz_info of string option * Span.t
    ]

  exception Error_exn of error

  let make ?tz ?ns ?s_frac ~iso_week_year ~iso_week ~weekday ~hour ~minute
      ~second () : (t, error) result =
    match Date.ISO_week_date.make ~iso_week_year ~iso_week ~weekday with
    | Error e -> Error (e :> error)
    | Ok date -> (
        match Time.make ~hour ~minute ~second ?ns ?s_frac () with
        | Error e -> Error (e :> error)
        | Ok time -> (
            match Zoneless'.to_zoned ?tz { date; time } with
            | Error e -> Error (e :> error)
            | Ok dt -> Ok dt))

  let make_exn ?tz ?ns ?s_frac ~iso_week_year ~iso_week ~weekday ~hour ~minute
      ~second () =
    match
      make ?tz ?ns ?s_frac ~iso_week_year ~iso_week ~weekday ~hour ~minute
        ~second ()
    with
    | Ok x -> x
    | Error e -> raise (Error_exn e)

  let make_unambiguous ?tz ?ns ?s_frac ~iso_week_year ~iso_week ~weekday ~hour
      ~minute ~second ~offset_from_utc () : (t, error) result =
    match Date.ISO_week_date.make ~iso_week_year ~iso_week ~weekday with
    | Error e -> Error (e :> error)
    | Ok date -> (
        match Time.make ~hour ~minute ~second ?ns ?s_frac () with
        | Error e -> Error (e :> error)
        | Ok time -> (
            match
              Zoneless'.to_zoned_unambiguous ?tz ~offset_from_utc { date; time }
            with
            | Error e -> Error (e :> error)
            | Ok dt -> Ok dt))

  let make_unambiguous_exn ?tz ?ns ?s_frac ~iso_week_year ~iso_week ~weekday
      ~hour ~minute ~second ~offset_from_utc () =
    match
      make_unambiguous ?tz ?ns ?s_frac ~iso_week_year ~iso_week ~weekday ~hour
        ~minute ~second ~offset_from_utc ()
    with
    | Ok x -> x
    | Error e -> raise (Error_exn e)
end
