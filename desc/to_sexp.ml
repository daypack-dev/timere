open Sexplib
open Date_time_utils

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

let sexp_of_tz_name t = Sexp.Atom (Time_zone.name t)

let sexp_of_span (x : Span.t) =
  Sexp.List
    [ sexp_of_int64 @@ Span.get_s x; sexp_of_int @@ Span.get_ns_offset x ]

let sexp_of_tz_info (info : Time_zone_info.t) =
  let tz = Time_zone_info.tz info in
  let fixed_offset_from_utc =
    Time_zone_info.fixed_offset_from_utc info
  in
  Sexp.List
    (List.filter_map Fun.id
       [
         Some (sexp_of_tz_name tz);
         Option.map
           (fun tz_offset ->
              sexp_of_int (Int64.to_int @@ Span.get_s tz_offset))
           fixed_offset_from_utc;
       ])

let sexp_of_date (x : Date.t) =
  let { Date.Ymd'.year; month; day } = Date.Ymd'.view x in
  Sexp.List [ sexp_of_int year; sexp_of_int month; sexp_of_int day ]

let sexp_of_time (x : Time.t) =
  let { Time.hour; minute; second; ns } = Time.view x in
  Sexp.List
    [ sexp_of_int hour; sexp_of_int minute; sexp_of_int second; sexp_of_int ns ]

let sexp_of_date_time (x : Date_time.t) =
  Sexp.List
    [
      sexp_of_date (Date_time.date x);
      sexp_of_time (Date_time.time x);
      sexp_of_tz_name (Date_time.tz x);
      (match Date_time.offset_from_utc x with
       | `Single offset ->
         Sexp.List
           [
             Sexp.Atom "single"; sexp_of_int (Int64.to_int @@ Span.get_s offset);
           ]
       | `Ambiguous (offset1, offset2) ->
         Sexp.List
           [
             Sexp.Atom "ambiguous";
             sexp_of_int (Int64.to_int @@ Span.get_s offset1);
             sexp_of_int (Int64.to_int @@ Span.get_s offset2);
           ]);
    ]

let sexp_of_zoneless (x : Date_time.Zoneless'.zoneless) =
  Sexp.List [ sexp_of_date Date_time.Zoneless'.(date x); sexp_of_time Date_time.Zoneless'.(time x) ]
