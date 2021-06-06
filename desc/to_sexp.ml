open Date_time_utils

let sexp_of_month x =
  CCSexp.atom
  @@ CCOpt.get_exn_or "Expected valid month"
  @@ abbr_string_of_month x

let sexp_of_weekday x = CCSexp.atom @@ abbr_string_of_weekday x

let sexp_of_int64 x = CCSexp.atom @@ Int64.to_string x

let sexp_of_int x = CCSexp.atom @@ string_of_int x

let sexp_list_of_ints l = List.map sexp_of_int l

let sexp_of_tz_name t = CCSexp.atom (Time_zone.name t)

let sexp_of_span (x : Span.t) =
  CCSexp.list
    [ sexp_of_int64 @@ Span.get_s x; sexp_of_int @@ Span.get_ns_offset x ]

let sexp_of_tz_info ({ tz; fixed_offset_from_utc } : Time_zone_info.t) =
  let open CCSexp in
  list
    (CCList.filter_map CCFun.id
       [
         Some (sexp_of_tz_name tz);
         CCOpt.map
           (fun tz_offset ->
             sexp_of_int (CCInt64.to_int @@ Span.get_s tz_offset))
           fixed_offset_from_utc;
       ])

let sexp_of_date (x : Date.t) =
  let open CCSexp in
  let { Date.Ymd_date.year; month; day } = Date.Ymd_date.view x in
  list [ sexp_of_int year; sexp_of_int month; sexp_of_int day ]

let sexp_of_time (x : Time.t) =
  let open CCSexp in
  let { Time.hour; minute; second; ns } = Time.view x in
  list
    [ sexp_of_int hour; sexp_of_int minute; sexp_of_int second; sexp_of_int ns ]

let sexp_of_date_time (x : Date_time.t) =
  let open CCSexp in
  list
    [
      sexp_of_date x.date;
      sexp_of_time x.time;
      sexp_of_tz_name x.tz;
      (match x.offset_from_utc with
      | `Single offset ->
          list
            [
              `Atom "single"; sexp_of_int (CCInt64.to_int @@ Span.get_s offset);
            ]
      | `Ambiguous (offset1, offset2) ->
          list
            [
              `Atom "ambiguous";
              sexp_of_int (CCInt64.to_int @@ Span.get_s offset1);
              sexp_of_int (CCInt64.to_int @@ Span.get_s offset2);
            ]);
    ]

let sexp_of_zoneless (x : Date_time.Zoneless'.zoneless) =
  let open CCSexp in
  list [ sexp_of_date x.date; sexp_of_time x.time ]
