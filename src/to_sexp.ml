let sexp_of_month x = CCSexp.atom @@ Time.abbr_string_of_month x

let sexp_of_weekday x = CCSexp.atom @@ Time.abbr_string_of_weekday x

let sexp_of_int x = CCSexp.atom @@ string_of_int x

let sexp_list_of_ints l = List.map sexp_of_int l

let sexp_of_tz t = CCSexp.atom (Time_zone.name t)

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
      list
        (match x.tz_info with
         | `Tz_only x -> [ atom "tz"; sexp_of_tz x ]
         | `Tz_offset_s_only x -> [ atom "tz_offset_s"; sexp_of_int x ]
         | `Tz_and_tz_offset_s (tz, tz_offset_s) ->
           [
             atom "tz_and_tz_offset_s"; sexp_of_tz tz; sexp_of_int tz_offset_s;
           ]);
    ]

let sexp_of_duration (x : Duration.t) =
  let open CCSexp in
  list
    [
      sexp_of_int x.days;
      sexp_of_int x.hours;
      sexp_of_int x.minutes;
      sexp_of_int x.seconds;
    ]

let sexp_of_timestamp x =
  x |> Time.Date_time.of_timestamp |> Result.get_ok |> sexp_of_date_time

let sexp_of_range ~(f : 'a -> CCSexp.t) (r : 'a Time.Range.range) =
  match r with
  | `Range_inc (x, y) -> CCSexp.(list [ atom "range_inc"; f x; f y ])
  | `Range_exc (x, y) -> CCSexp.(list [ atom "range_exc"; f x; f y ])

let sexp_of_pattern (pat : Time.Pattern.t) : CCSexp.t =
  let years = pat.years |> Int_set.to_seq |> List.of_seq |> sexp_list_of_ints in
  let months =
    pat.months |> Time.Month_set.to_seq |> List.of_seq |> List.map sexp_of_month
  in
  let month_days =
    pat.month_days |> Int_set.to_seq |> List.of_seq |> sexp_list_of_ints
  in
  let weekdays =
    pat.weekdays
    |> Time.Weekday_set.to_seq
    |> List.of_seq
    |> List.map sexp_of_weekday
  in
  let hours = pat.hours |> Int_set.to_seq |> List.of_seq |> sexp_list_of_ints in
  let minutes =
    pat.minutes |> Int_set.to_seq |> List.of_seq |> sexp_list_of_ints
  in
  let seconds =
    pat.seconds |> Int_set.to_seq |> List.of_seq |> sexp_list_of_ints
  in
  let open CCSexp in
  [
    Some (atom "pattern");
    (match years with [] -> None | _ -> Some (list (atom "years" :: years)));
    (match months with
     | [] -> None
     | _ -> Some (list (atom "months" :: months)));
    (match month_days with
     | [] -> None
     | _ -> Some (list (atom "month_days" :: month_days)));
    (match weekdays with
     | [] -> None
     | _ -> Some (list (atom "weekdays" :: weekdays)));
    (match hours with [] -> None | _ -> Some (list (atom "hours" :: hours)));
    (match minutes with
     | [] -> None
     | _ -> Some (list (atom "minutes" :: minutes)));
    (match seconds with
     | [] -> None
     | _ -> Some (list (atom "seconds" :: seconds)));
  ]
  |> List.filter_map (fun x -> x)
  |> list

let sexp_list_of_unary_op (op : Time.unary_op) =
  let open Time in
  match op with
  | Not -> [ CCSexp.atom "not" ]
  | Every -> [ CCSexp.atom "every" ]
  | Drop_n_points n ->
    [ CCSexp.atom "drop_n_points"; CCSexp.atom (string_of_int n) ]
  | Take_n_points n ->
    [ CCSexp.atom "take_n_points"; CCSexp.atom (string_of_int n) ]
  (* | Every_nth n -> [ CCSexp.atom "every_nth"; CCSexp.atom (string_of_int n) ]
   * | Nth n -> [ CCSexp.atom "nth"; CCSexp.atom (string_of_int n) ]
   * | Chunk { chunk_size; drop_partial } ->
   *   [
   *     Some (CCSexp.atom "chunk");
   *     (if drop_partial then Some (CCSexp.atom "drop_partial") else None);
   *     Some (CCSexp.atom (Int64.to_string chunk_size));
   *   ]
   *   |> List.filter_map (fun x -> x)
   * | Chunk_by_year -> [ CCSexp.atom "chunk_by_year"]
   * | Chunk_by_month -> [ CCSexp.atom "chunk_by_month"] *)
  | Shift n -> [ CCSexp.atom "shift"; CCSexp.atom (Int64.to_string n) ]
  | Lengthen n -> [ CCSexp.atom "lengthen"; CCSexp.atom (Int64.to_string n) ]
  | With_tz tz -> [ CCSexp.atom "with_tz"; CCSexp.atom (Time_zone.name tz) ]

let to_sexp (t : Time.t) : CCSexp.t =
  let open Time in
  let rec aux t =
    match t with
    | All -> CCSexp.(list [ atom "all" ])
    | Empty -> CCSexp.(list [ atom "empty" ])
    | Timestamp_interval_seq (_, s) ->
      let l =
        s
        |> List.of_seq
        |> List.map (fun (x, y) ->
            CCSexp.list [ sexp_of_timestamp x; sexp_of_timestamp y ])
      in
      CCSexp.list (CCSexp.atom "intervals" :: l)
    | Pattern (_, pat) -> sexp_of_pattern pat
    | Unary_op (_, op, t) -> CCSexp.list (sexp_list_of_unary_op op @ [ aux t ])
    | Interval_inc (_, a, b) ->
      let open CCSexp in
      list [ atom "interval_inc"; sexp_of_timestamp a; sexp_of_timestamp b ]
    | Interval_exc (_, a, b) ->
      let open CCSexp in
      list [ atom "interval_exc"; sexp_of_timestamp a; sexp_of_timestamp b ]
    | Round_robin_pick_list (_, l) ->
      CCSexp.(list (atom "round_robin" :: List.map aux l))
    | Inter_seq (_, s) ->
      CCSexp.(list (atom "inter" :: (s |> Seq.map aux |> List.of_seq)))
    | Union_seq (_, s) ->
      CCSexp.(list (atom "union" :: (s |> Seq.map aux |> List.of_seq)))
    | After (_, b, t1, t2) ->
      CCSexp.(
        list
          [
            atom "after";
            Duration.of_seconds b |> sexp_of_duration;
            aux t1;
            aux t2;
          ])
    | Between_inc (_, b, t1, t2) ->
      CCSexp.(
        list
          [
            atom "between_inc";
            Duration.of_seconds b |> sexp_of_duration;
            aux t1;
            aux t2;
          ])
    | Between_exc (_, b, t1, t2) ->
      CCSexp.(
        list
          [
            atom "between_exc";
            Duration.of_seconds b |> sexp_of_duration;
            aux t1;
            aux t2;
          ])
    | Unchunk chunked -> CCSexp.(list [ atom "unchunk"; aux_chunked chunked ])
  and aux_chunked chunked =
    let sexp_list_of_unary_op_on_t op =
      let open CCSexp in
      match op with
      | Chunk_disjoint_interval -> [ atom "chunk_disjoint_intervals" ]
      | Chunk_at_year_boundary -> [ atom "chunk_at_year_boundary" ]
      | Chunk_at_month_boundary -> [ atom "chunk_at_month_boundary" ]
      | Chunk_by_duration { chunk_size; drop_partial } ->
        [
          Some (atom "chunk_by_duration");
          Some (sexp_of_duration (Duration.of_seconds chunk_size));
          (if drop_partial then Some (atom "drop_partial") else None);
        ]
        |> List.filter_map (fun x -> x)
    in
    match chunked with
    | Unary_op_on_t (op, t) ->
      CCSexp.(list (sexp_list_of_unary_op_on_t op @ [ aux t ]))
    | Unary_op_on_chunked (op, chunked) ->
      CCSexp.(
        list
          (match op with
           | Drop n -> [ atom "drop"; sexp_of_int n; aux_chunked chunked ]
           | Take n -> [ atom "drop"; sexp_of_int n; aux_chunked chunked ]
           | Take_nth n ->
             [ atom "take_nth"; sexp_of_int n; aux_chunked chunked ]
           | Nth n -> [ atom "nth"; sexp_of_int n; aux_chunked chunked ]
           | Chunk_again op ->
             [
               atom "chunk_again";
               list (sexp_list_of_unary_op_on_t op @ [ aux_chunked chunked ]);
             ]))
  in
  aux t

let to_sexp_string t = CCSexp.to_string (to_sexp t)