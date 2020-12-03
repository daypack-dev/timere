let sexp_of_month x = CCSexp.atom @@ Time.abbr_string_of_month x

let sexp_of_weekday x = CCSexp.atom @@ Time.abbr_string_of_weekday x

let sexp_of_int x = CCSexp.atom @@ string_of_int x

let sexp_list_of_ints l = List.map sexp_of_int l

let sexp_of_date_time (x : Time.Date_time.t) =
  let open CCSexp in
  list
    [
      sexp_of_int x.year;
      sexp_of_month x.month;
      sexp_of_int x.day;
      sexp_of_int x.hour;
      sexp_of_int x.minute;
      sexp_of_int x.second;
      list [ atom "tz_offset_s"; sexp_of_int x.tz_offset_s ];
    ]

let sexp_of_timestamp x =
  x |> Time.Date_time.of_timestamp |> Result.get_ok |> sexp_of_date_time

let sexp_of_range ~(f : 'a -> CCSexp.t) (r : 'a Time.Range.range) =
  match r with
  | `Range_inc (x, y) -> CCSexp.(list [ atom "range_inc"; f x; f y ])
  | `Range_exc (x, y) -> CCSexp.(list [ atom "range_exc"; f x; f y ])

let sexp_of_pattern (pat : Time.Pattern.pattern) : CCSexp.t =
  let years = sexp_list_of_ints pat.years in
  let months = List.map sexp_of_month pat.months in
  let month_days = sexp_list_of_ints pat.month_days in
  let weekdays = List.map sexp_of_weekday pat.weekdays in
  let hours = sexp_list_of_ints pat.hours in
  let minutes = sexp_list_of_ints pat.minutes in
  let seconds = sexp_list_of_ints pat.seconds in
  let timestamps = List.map sexp_of_timestamp pat.timestamps in
  let open CCSexp in
  [
    Some (atom "pattern");
    (match years with [] -> None | _ -> Some (list (atom "years" :: years)));
    (match months with [] -> None | _ -> Some (list (atom "months" :: months)));
    ( match month_days with
      | [] -> None
      | _ -> Some (list (atom "month_days" :: month_days)) );
    ( match weekdays with
      | [] -> None
      | _ -> Some (list (atom "weekdays" :: weekdays)) );
    (match hours with [] -> None | _ -> Some (list (atom "hours" :: hours)));
    ( match minutes with
      | [] -> None
      | _ -> Some (list (atom "minutes" :: minutes)) );
    ( match seconds with
      | [] -> None
      | _ -> Some (list (atom "seconds" :: seconds)) );
    ( match timestamps with
      | [] -> None
      | _ -> Some (list (atom "timestamps" :: timestamps)) );
  ]
  |> List.filter_map (fun x -> x)
  |> list

let sexp_of_branching (b : Time.branching) : CCSexp.t =
  let open Time in
  let years = List.map (sexp_of_range ~f:sexp_of_int) b.years in
  let months = List.map (sexp_of_range ~f:sexp_of_month) b.months in
  let days =
    match b.days with
    | Month_days days -> (
        match days with
        | [] -> []
        | _ ->
          CCSexp.atom "month_days"
          :: List.map (sexp_of_range ~f:sexp_of_int) days )
    | Weekdays days -> (
        match days with
        | [] -> []
        | _ ->
          CCSexp.atom "weekdays"
          :: List.map (sexp_of_range ~f:sexp_of_weekday) days )
  in
  let hmss =
    List.map
      (sexp_of_range ~f:(fun { hour; minute; second } ->
           let open CCSexp in
           list [ sexp_of_int hour; sexp_of_int minute; sexp_of_int second ]))
      b.hmss
  in
  let open CCSexp in
  [
    Some (atom "branching");
    (match years with [] -> None | _ -> Some (list (atom "years" :: years)));
    (match months with [] -> None | _ -> Some (list (atom "months" :: months)));
    (match days with [] -> None | _ -> Some (list days));
    (match hmss with [] -> None | _ -> Some (list (atom "hmss" :: hmss)));
  ]
  |> List.filter_map (fun x -> x)
  |> list

let sexp_list_of_unary_op (op : Time.unary_op) =
  let open Time in
  match op with
  | Not -> [ CCSexp.atom "not" ]
  | Every -> [ CCSexp.atom "every" ]
  | Skip_n_points n ->
    [ CCSexp.atom "skip_n_points"; CCSexp.atom (string_of_int n) ]
  | Skip_n_intervals n ->
    [ CCSexp.atom "skip_n"; CCSexp.atom (string_of_int n) ]
  | Next_n_points n ->
    [ CCSexp.atom "next_n_points"; CCSexp.atom (string_of_int n) ]
  | Next_n_intervals n ->
    [ CCSexp.atom "next_n"; CCSexp.atom (string_of_int n) ]
  | Chunk { chunk_size; drop_partial } ->
    [
      Some (CCSexp.atom "chunk");
      (if drop_partial then Some (CCSexp.atom "drop_partial") else None);
      Some (CCSexp.atom (Int64.to_string chunk_size));
    ]
    |> List.filter_map (fun x -> x)
  | Shift n -> [ CCSexp.atom "shift"; CCSexp.atom (Int64.to_string n) ]
  | Lengthen n -> [ CCSexp.atom "lengthen"; CCSexp.atom (Int64.to_string n) ]
  | Tz_offset_s n ->
    [ CCSexp.atom "change_tz_offset_s"; CCSexp.atom (string_of_int n) ]

let to_sexp (t : Time.t) : CCSexp.t =
  let open Time in
  let rec aux t =
    match t with
    | Timestamp_interval_seq (_, s) ->
      let l =
        s
        |> List.of_seq
        |> List.map (fun (x, y) ->
            CCSexp.list [ sexp_of_timestamp x; sexp_of_timestamp y ])
      in
      CCSexp.list (CCSexp.atom "intervals" :: l)
    | Pattern (_, pat) -> sexp_of_pattern pat
    | Branching (_, b) -> sexp_of_branching b
    | Unary_op (_, op, t) -> CCSexp.list (sexp_list_of_unary_op op @ [ aux t ])
    | Interval_inc (_, a, b) ->
      let open CCSexp in
      list [ atom "interval_inc"; sexp_of_timestamp a; sexp_of_timestamp b ]
    | Interval_exc (_, a, b) ->
      let open CCSexp in
      list [ atom "interval_exc"; sexp_of_timestamp a; sexp_of_timestamp b ]
    | Round_robin_pick_list (_, l) ->
      CCSexp.(list (atom "round_robin" :: List.map aux l))
    | Inter_list (_, l) -> CCSexp.(list (atom "inter" :: List.map aux l))
    | Union_list (_, l) -> CCSexp.(list (atom "union" :: List.map aux l))
  in
  aux t

let to_sexp_string t = CCSexp.to_string (to_sexp t)
