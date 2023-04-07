open Sexplib
open Date_time_utils
module T = Timedesc

let sexp_of_month x =
  Sexp.Atom
    (match abbr_string_of_month x with
     | Some x -> x
     | None -> invalid_arg "Expected valid month"
    )

let sexp_of_weekday x = Sexp.Atom (abbr_string_of_weekday x)

let sexp_of_int64 x = Sexp.Atom (Int64.to_string x)

let sexp_of_int x = Sexp.Atom (string_of_int x)

let sexp_list_of_ints l = List.map sexp_of_int l

let sexp_of_tz_name t = Sexp.Atom (T.Time_zone.name t)

let sexp_of_span (x : T.Span.t) =
  Sexp.List
    [ sexp_of_int64 @@ T.Span.get_s x; sexp_of_int @@ T.Span.get_ns_offset x ]

let sexp_of_tz_info (info : T.Time_zone_info.t) =
  let tz = T.Time_zone_info.tz info in
  let offset_from_utc =
    T.Time_zone_info.offset_from_utc info
  in
  Sexp.List
    (List.filter_map Fun.id
       [
         Some (sexp_of_tz_name tz);
         Option.map
           (fun tz_offset ->
              sexp_of_int (Int64.to_int @@ T.Span.get_s tz_offset))
           offset_from_utc;
       ])

let sexp_of_date (x : T.Date.t) =
  let { T.Date.Ymd.year; month; day } = T.Date.Ymd.view x in
  Sexp.List [ sexp_of_int year; sexp_of_int month; sexp_of_int day ]

let sexp_of_time (x : T.Time.t) =
  let { T.Time.hour; minute; second; ns } = T.Time.view x in
  Sexp.List
    [ sexp_of_int hour; sexp_of_int minute; sexp_of_int second; sexp_of_int ns ]

let sexp_of_date_time (x : T.t) =
  Sexp.List
    [
      sexp_of_date (T.date x);
      sexp_of_time (T.time x);
      sexp_of_tz_name (T.tz x);
      (match T.offset_from_utc x with
       | `Single offset ->
         Sexp.List
           [
             Sexp.Atom "single"; sexp_of_int (Int64.to_int @@ T.Span.get_s offset);
           ]
       | `Ambiguous (offset1, offset2) ->
         Sexp.List
           [
             Sexp.Atom "ambiguous";
             sexp_of_int (Int64.to_int @@ T.Span.get_s offset1);
             sexp_of_int (Int64.to_int @@ T.Span.get_s offset2);
           ]);
    ]

let sexp_of_zoneless (x : T.Zoneless.zoneless) =
  Sexp.List [ sexp_of_date T.Zoneless.(date x); sexp_of_time T.Zoneless.(time x) ]
