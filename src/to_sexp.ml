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
  | Drop_n_points n ->
    [ CCSexp.atom "skip_n_points"; CCSexp.atom (string_of_int n) ]
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
  | Change_tz_offset_s n ->
    [ CCSexp.atom "change_tz_offset_s"; CCSexp.atom (string_of_int n) ]

let sexp_of_recur (r : Time.recur) : CCSexp.t =
  let open Time in
  CCSexp.(
    list
      (List.filter_map
         (fun x -> x)
         [
           Some (atom "recur");
           Some (list [ atom "start"; sexp_of_date_time r.start ]);
           Option.map
             (fun year ->
                list
                  [
                    atom "year";
                    ( match year with
                      | Match l -> list (atom "match" :: sexp_list_of_ints l)
                      | Every_nth n -> list [ atom "every_nth"; sexp_of_int n ] );
                  ])
             r.year;
           Option.map
             (fun month ->
                list
                  [
                    atom "month";
                    ( match month with
                      | Match l -> list (atom "match" :: List.map sexp_of_month l)
                      | Every_nth n -> list [ atom "every_nth"; sexp_of_int n ] );
                  ])
             r.month;
           Option.map
             (fun day ->
                list
                  [
                    atom "day";
                    ( match day with
                      | Day (Match l) -> list (atom "match" :: sexp_list_of_ints l)
                      | Day (Every_nth n) ->
                        list [ atom "every_nth"; sexp_of_int n ]
                      | Weekday_every_nth (n, weekday) ->
                        list
                          [
                            atom "weekday_every_nth";
                            sexp_of_int n;
                            sexp_of_weekday weekday;
                          ]
                      | Weekday_nth (n, weekday) ->
                        list
                          [
                            atom "weekday_nth";
                            sexp_of_int n;
                            sexp_of_weekday weekday;
                          ] );
                  ])
             r.day;
         ]))

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
    | Branching (_, b) -> sexp_of_branching b
    | Recur (_, r) -> sexp_of_recur r
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
    | After (_, t1, t2) -> CCSexp.(list [ atom "after"; aux t1; aux t2 ])
    | Between_inc (_, t1, t2) ->
      CCSexp.(list [ atom "between_inc"; aux t1; aux t2 ])
    | Between_exc (_, t1, t2) ->
      CCSexp.(list [ atom "between_exc"; aux t1; aux t2 ])
    | Unchunk chunked -> CCSexp.(list [ atom "unchunk"; aux_chunked chunked ])
  and aux_chunked chunked =
    let sexp_list_of_unary_op_on_t op =
      let open CCSexp in
      match op with
      | Chunk_disjoint_interval -> [ atom "chunk_disjoint_interval" ]
      | Chunk_at_year_boundary -> [ atom "chunk_at_year_boundary" ]
      | Chunk_at_month_boundary -> [ atom "chunk_at_month_boundary" ]
      | Chunk_by_duration { chunk_size; drop_partial } ->
        [
          Some (atom "chunk_by_duration");
          Some
            (sexp_of_duration
               (Result.get_ok @@ Duration.of_seconds chunk_size));
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
          ( match op with
            | Drop n -> [ atom "drop"; sexp_of_int n; aux_chunked chunked ]
            | Take n -> [ atom "drop"; sexp_of_int n; aux_chunked chunked ]
            | Take_nth n ->
              [ atom "take_nth"; sexp_of_int n; aux_chunked chunked ]
            | Nth n -> [ atom "nth"; sexp_of_int n; aux_chunked chunked ]
            | Chunk_again op ->
              [
                atom "chunk_again";
                list (sexp_list_of_unary_op_on_t op @ [ aux_chunked chunked ]);
              ] ))
  in
  aux t

let to_sexp_string t = CCSexp.to_string (to_sexp t)
