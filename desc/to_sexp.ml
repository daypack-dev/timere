open Date_time_components

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
  CCSexp.list [ sexp_of_int64 x.s; sexp_of_int x.ns ]

let sexp_of_tz_info ({ tz; fixed_offset_from_utc } : Time_zone_info.t) =
  let open CCSexp in
  list
    (CCList.filter_map CCFun.id
       [
         Some (sexp_of_tz_name tz);
         CCOpt.map
           (fun tz_offset -> sexp_of_int (CCInt64.to_int Span.(tz_offset.s)))
           fixed_offset_from_utc;
       ])

let sexp_of_date_time (x : Date_time.t) =
  let open CCSexp in
  let { Date.Ymd_date.year; month; day } = Date_time.ymd_date x in
  let { Time.hour; minute; second; ns } = Date_time.time_view x in
  list
    [
      sexp_of_int year;
      sexp_of_int month;
      sexp_of_int day;
      sexp_of_int hour;
      sexp_of_int minute;
      sexp_of_int second;
      sexp_of_int ns;
      sexp_of_tz_name x.tz;
      (match x.offset_from_utc with
       | `Single offset ->
         list [ `Atom "single"; sexp_of_int (CCInt64.to_int Span.(offset.s)) ]
       | `Ambiguous (offset1, offset2) ->
         list
           [
             `Atom "ambiguous";
             sexp_of_int (CCInt64.to_int Span.(offset1.s));
             sexp_of_int (CCInt64.to_int Span.(offset2.s));
           ]);
    ]

let sexp_of_timestamp x =
  x
  |> Date_time.of_timestamp ~tz_of_date_time:Time_zone.utc
  |> CCOpt.get_exn_or "expected successful date time construction"
  |> sexp_of_date_time
