type t = {
  tz : Time_zone.t;
  offset_from_utc : Span.t option;
}

let tz (x : t) = x.tz

let offset_from_utc (x : t) = x.offset_from_utc

let equal (x : t) (y : t) =
  match (x, y) with
  | ( { tz = tz1; offset_from_utc = None },
      { tz = tz2; offset_from_utc = None } ) ->
    Time_zone.equal tz1 tz2
  | ( { tz = tz1; offset_from_utc = Some x1 },
      { tz = tz2; offset_from_utc = Some x2 } ) ->
    Time_zone.equal tz1 tz2 && Span.equal x1 x2
  | _, _ -> false

type error =
  [ `Missing_both_tz_and_offset_from_utc
  | `Invalid_offset of Span.t
  | `Unrecorded_offset of Span.t
  ]

let make ?tz ?offset_from_utc () : (t, error) result =
  let offset_from_utc =
    Option.map
      (fun offset -> Span.(make ~s:(Span.get_s offset) ()))
      offset_from_utc
  in
  match (tz, offset_from_utc) with
  | None, None -> Error `Missing_both_tz_and_offset_from_utc
  | Some tz, None ->
    Ok { tz; offset_from_utc = Time_zone.to_fixed_offset_from_utc tz }
  | None, Some offset_from_utc -> (
      match Time_zone.make_offset_only offset_from_utc with
      | None -> Error (`Invalid_offset offset_from_utc)
      | Some tz -> Ok { tz; offset_from_utc = Some offset_from_utc })
  | Some tz, Some offset_from_utc ->
    if Time_zone.offset_is_recorded offset_from_utc tz then
      Ok { tz; offset_from_utc = Some offset_from_utc }
    else Error (`Unrecorded_offset offset_from_utc)

let make_exn ?tz ?offset_from_utc () : t =
  match make ?tz ?offset_from_utc () with
  | Ok x -> x
  | Error _ -> invalid_arg "make_exn"
