open Date_time_components

let sexp_of_month x =
  CCSexp.atom @@ CCOpt.get_exn @@ Time.abbr_string_of_month x

let sexp_of_weekday x = CCSexp.atom @@ Time.abbr_string_of_weekday x

let sexp_of_int64 x = CCSexp.atom @@ Int64.to_string x

let sexp_of_int x = CCSexp.atom @@ string_of_int x

let sexp_list_of_ints l = List.map sexp_of_int l

let sexp_of_tz_name t = CCSexp.atom (Time_zone.name t)

let sexp_of_span (x : Span.t) =
  CCSexp.list [ sexp_of_int64 x.s; sexp_of_int x.ns ]

let sexp_of_tz_info ({ tz; offset_from_utc } : tz_info) =
  let open CCSexp in
  list
    (CCList.filter_map CCFun.id
       [
         Some (sexp_of_tz_name tz);
         CCOpt.map
           (fun tz_offset -> sexp_of_int (CCInt64.to_int Span.(tz_offset.s)))
           offset_from_utc;
       ])

let sexp_of_date_time (x : Time.t) =
  let open CCSexp in
  list
    [
      sexp_of_int x.year;
      sexp_of_int x.day_of_year;
      sexp_of_int x.hour;
      sexp_of_int x.minute;
      sexp_of_int x.second;
      sexp_of_int x.ns;
      sexp_of_tz_info x.tz_info;
    ]

let sexp_of_timestamp x =
  x
  |> Time.Odt'.of_timestamp ~tz_of_date_time:Time_zone.utc
  |> CCOpt.get_exn
  |> sexp_of_date_time
