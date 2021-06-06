let sexp_of_month x =
  CCSexp.atom
  @@ CCOpt.get_exn_or "Expected valid month"
  @@ Time.abbr_string_of_month x

let sexp_of_weekday x = CCSexp.atom @@ Time.abbr_string_of_weekday x

let sexp_of_int64 x = CCSexp.atom @@ Int64.to_string x

let sexp_of_int x = CCSexp.atom @@ string_of_int x

let sexp_list_of_ints l = List.map sexp_of_int l

let sexp_of_tz_name t = CCSexp.atom (Timedesc.Time_zone.name t)

let sexp_of_span = Timedesc.Span.to_sexp

let sexp_of_points ({ pick; tz_info } : Points.t) =
  let open CCSexp in
  let open Points in
  list
    (CCList.filter_map CCFun.id
       [
         Some (atom "points");
         Some
           (list
              (atom "pick"
               ::
               (match pick with
               | N ns -> [ atom "n"; sexp_of_int ns ]
               | SN { second; ns } ->
                   [ atom "sn"; sexp_of_int second; sexp_of_int ns ]
               | MSN { minute; second; ns } ->
                   [
                     atom "msn";
                     sexp_of_int minute;
                     sexp_of_int second;
                     sexp_of_int ns;
                   ]
               | HMSN { hour; minute; second; ns } ->
                   [
                     atom "hmsn";
                     sexp_of_int hour;
                     sexp_of_int minute;
                     sexp_of_int second;
                     sexp_of_int ns;
                   ]
               | WHMSN { weekday; hour; minute; second; ns } ->
                   [
                     atom "whmsn";
                     sexp_of_weekday weekday;
                     sexp_of_int hour;
                     sexp_of_int minute;
                     sexp_of_int second;
                     sexp_of_int ns;
                   ]
               | DHMSN { month_day; hour; minute; second; ns } ->
                   [
                     atom "dhmsn";
                     sexp_of_int month_day;
                     sexp_of_int hour;
                     sexp_of_int minute;
                     sexp_of_int second;
                     sexp_of_int ns;
                   ]
               | MDHMSN { month; month_day; hour; minute; second; ns } ->
                   [
                     atom "mdhmsn";
                     sexp_of_month month;
                     sexp_of_int month_day;
                     sexp_of_int hour;
                     sexp_of_int minute;
                     sexp_of_int second;
                     sexp_of_int ns;
                   ]
               | YMDHMSN { year; month; month_day; hour; minute; second; ns } ->
                   [
                     atom "ymdhmsn";
                     sexp_of_int year;
                     sexp_of_month month;
                     sexp_of_int month_day;
                     sexp_of_int hour;
                     sexp_of_int minute;
                     sexp_of_int second;
                     sexp_of_int ns;
                   ])));
         CCOpt.map Timedesc.Time_zone_info.to_sexp tz_info;
       ])

let sexp_of_date_time = Timedesc.to_sexp

let sexp_of_timestamp = Timedesc.Timestamp.to_sexp

let sexp_of_range ~(f : 'a -> CCSexp.t) (r : 'a Time.Range.range) =
  match r with
  | `Range_inc (x, y) -> CCSexp.(list [ atom "range_inc"; f x; f y ])
  | `Range_exc (x, y) -> CCSexp.(list [ atom "range_exc"; f x; f y ])

let sexp_of_pattern (pat : Pattern.t) : CCSexp.t =
  let years =
    pat.years |> Int_set.to_seq |> CCList.of_seq |> sexp_list_of_ints
  in
  let months =
    pat.months |> Int_set.to_seq |> CCList.of_seq |> List.map sexp_of_month
  in
  let month_days =
    pat.month_days |> Int_set.to_seq |> CCList.of_seq |> sexp_list_of_ints
  in
  let weekdays =
    pat.weekdays
    |> Weekday_set.to_seq
    |> CCList.of_seq
    |> List.map sexp_of_weekday
  in
  let hours =
    pat.hours |> Int_set.to_seq |> CCList.of_seq |> sexp_list_of_ints
  in
  let minutes =
    pat.minutes |> Int_set.to_seq |> CCList.of_seq |> sexp_list_of_ints
  in
  let seconds =
    pat.seconds |> Int_set.to_seq |> CCList.of_seq |> sexp_list_of_ints
  in
  let ns =
    Diet.Int.fold
      (fun interval acc ->
        (Diet.Int.Interval.x interval, Diet.Int.Interval.y interval) :: acc)
      pat.ns []
    |> List.rev
    |> List.map (fun (x, y) -> `List [ sexp_of_int x; sexp_of_int y ])
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
    (match ns with [] -> None | _ -> Some (list (atom "ns" :: ns)));
  ]
  |> CCList.filter_map CCFun.id
  |> list

let sexp_list_of_unary_op (op : Time_ast.unary_op) =
  match op with
  | Not -> [ CCSexp.atom "not" ]
  | Shift n -> [ CCSexp.atom "shift"; sexp_of_span n ]
  | Lengthen n -> [ CCSexp.atom "lengthen"; sexp_of_span n ]
  | With_tz tz ->
      [ CCSexp.atom "with_tz"; CCSexp.atom (Timedesc.Time_zone.name tz) ]

let to_sexp (t : Time_ast.t) : CCSexp.t =
  let rec aux (t : Time_ast.t) =
    match t with
    | All -> CCSexp.(list [ atom "all" ])
    | Empty -> CCSexp.(list [ atom "empty" ])
    | Intervals s ->
        let l =
          s
          |> CCList.of_seq
          |> List.map (fun (x, y) ->
                 CCSexp.list [ sexp_of_timestamp x; sexp_of_timestamp y ])
        in
        CCSexp.list (CCSexp.atom "intervals" :: l)
    | Pattern pat -> sexp_of_pattern pat
    | Unary_op (op, t) -> CCSexp.list (sexp_list_of_unary_op op @ [ aux t ])
    | Inter_seq s ->
        CCSexp.(list (atom "inter" :: (s |> Seq.map aux |> CCList.of_seq)))
    | Union_seq s ->
        CCSexp.(list (atom "union" :: (s |> Seq.map aux |> CCList.of_seq)))
    | Pattern_intervals { mode; bound; start; end_ } ->
        CCSexp.(
          list
            [
              atom "pattern_intervals";
              (match mode with
              | `Whole_inc -> atom "whole_inc"
              | `Whole_exc -> atom "whole_exc"
              | `Fst -> atom "fst"
              | `Snd -> atom "snd");
              sexp_of_span bound;
              sexp_of_points start;
              sexp_of_points end_;
            ])
    | Unchunk chunked -> CCSexp.(list [ atom "unchunk"; aux_chunked chunked ])
  and aux_chunked chunked =
    let sexp_list_of_unary_op_on_t op =
      let open CCSexp in
      match op with
      | Time_ast.Chunk_disjoint_interval -> [ atom "chunk_disjoint_intervals" ]
      | Chunk_at_year_boundary -> [ atom "chunk_at_year_boundary" ]
      | Chunk_at_month_boundary -> [ atom "chunk_at_month_boundary" ]
      | Chunk_by_duration { chunk_size; drop_partial } ->
          [
            Some (atom "chunk_by_duration");
            Some (sexp_of_span chunk_size);
            (if drop_partial then Some (atom "drop_partial") else None);
          ]
          |> CCList.filter_map CCFun.id
    in
    match chunked with
    | Unary_op_on_t (op, t) ->
        CCSexp.(list (sexp_list_of_unary_op_on_t op @ [ aux t ]))
    | Unary_op_on_chunked (op, chunked) ->
        CCSexp.(
          list
            (match op with
            | Drop n -> [ atom "drop"; sexp_of_int n; aux_chunked chunked ]
            | Take n -> [ atom "take"; sexp_of_int n; aux_chunked chunked ]
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

let wrap_to_sexp_into_to_sexp_string (f : 'a -> CCSexp.t) x =
  CCSexp.to_string (f x)

let to_sexp_string = wrap_to_sexp_into_to_sexp_string to_sexp
