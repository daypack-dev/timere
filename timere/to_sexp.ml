open Sexplib

let sexp_of_month x =
  Sexp.Atom
    (CCOption.get_exn_or "Expected valid month"
     @@ Time.abbr_string_of_month x)

let sexp_of_weekday x = Sexp.Atom (Time.abbr_string_of_weekday x)

let sexp_of_int64 x = Sexp.Atom (Int64.to_string x)

let sexp_of_int x = Sexp.Atom (string_of_int x)

let sexp_list_of_ints l = List.map sexp_of_int l

let sexp_of_tz_name t = Sexp.Atom (Timedesc.Time_zone.name t)

let sexp_of_span = Timedesc_sexp.Span.to_sexp

let sexp_of_points ({ pick; tz_info } : Points.t) =
  let open Points in
  Sexp.List
    (CCList.filter_map CCFun.id
       [
         Some (Sexp.Atom "points");
         Some
           (Sexp.List
              (Sexp.Atom "pick"
               ::
               (match pick with
                | N ns -> [ Atom "n"; sexp_of_int ns ]
                | SN { second; ns } ->
                  [ Atom "sn"; sexp_of_int second; sexp_of_int ns ]
                | MSN { minute; second; ns } ->
                  [
                    Atom "msn";
                    sexp_of_int minute;
                    sexp_of_int second;
                    sexp_of_int ns;
                  ]
                | HMSN { hour; minute; second; ns } ->
                  [
                    Atom "hmsn";
                    sexp_of_int hour;
                    sexp_of_int minute;
                    sexp_of_int second;
                    sexp_of_int ns;
                  ]
                | WHMSN { weekday; hour; minute; second; ns } ->
                  [
                    Atom "whmsn";
                    sexp_of_weekday weekday;
                    sexp_of_int hour;
                    sexp_of_int minute;
                    sexp_of_int second;
                    sexp_of_int ns;
                  ]
                | DHMSN { month_day; hour; minute; second; ns } ->
                  [
                    Atom "dhmsn";
                    sexp_of_int month_day;
                    sexp_of_int hour;
                    sexp_of_int minute;
                    sexp_of_int second;
                    sexp_of_int ns;
                  ]
                | MDHMSN { month; month_day; hour; minute; second; ns } ->
                  [
                    Atom "mdhmsn";
                    sexp_of_month month;
                    sexp_of_int month_day;
                    sexp_of_int hour;
                    sexp_of_int minute;
                    sexp_of_int second;
                    sexp_of_int ns;
                  ]
                | YMDHMSN { year; month; month_day; hour; minute; second; ns } ->
                  [
                    Atom "ymdhmsn";
                    sexp_of_int year;
                    sexp_of_month month;
                    sexp_of_int month_day;
                    sexp_of_int hour;
                    sexp_of_int minute;
                    sexp_of_int second;
                    sexp_of_int ns;
                  ])));
         CCOption.map Timedesc_sexp.Time_zone_info.to_sexp tz_info;
       ])

let sexp_of_date_time = Timedesc_sexp.to_sexp

let sexp_of_timestamp = Timedesc_sexp.Timestamp.to_sexp

let sexp_of_range ~(f : 'a -> Sexp.t) (r : 'a Time.Range.range) =
  match r with
  | `Range_inc (x, y) -> Sexp.(List [ Atom "range_inc"; f x; f y ])
  | `Range_exc (x, y) -> Sexp.(List [ Atom "range_exc"; f x; f y ])

let sexp_of_iso_week_pattern (years : Int_set.t) (weeks : Int_set.t) : Sexp.t =
  let years = Int_set.to_list years |> sexp_list_of_ints in
  let weeks = Int_set.to_list weeks |> sexp_list_of_ints in
  Sexp.(List [Atom "iso_week_pattern"; List years; List weeks])

let sexp_of_pattern (pat : Pattern.t) : Sexp.t =
  let years =
    pat.years |> Int_set.to_list |> sexp_list_of_ints
  in
  let months =
    pat.months |> Int_set.to_list |> List.map sexp_of_month
  in
  let month_days =
    pat.month_days |> Int_set.to_list |> sexp_list_of_ints
  in
  let weekdays =
    pat.weekdays
    |> Weekday_set.to_list
    |> List.map sexp_of_weekday
  in
  let hours =
    pat.hours |> Int_set.to_list |> sexp_list_of_ints
  in
  let minutes =
    pat.minutes |> Int_set.to_list |> sexp_list_of_ints
  in
  let seconds =
    pat.seconds |> Int_set.to_list |> sexp_list_of_ints
  in
  let ns =
    Diet.Int.fold
      (fun interval acc ->
         (Diet.Int.Interval.x interval, Diet.Int.Interval.y interval) :: acc)
      pat.ns []
    |> List.rev
    |> List.map (fun (x, y) -> Sexp.List [ sexp_of_int x; sexp_of_int y ])
  in
  Sexp.List
    (
      CCList.filter_map CCFun.id
        [
          Some (Sexp.Atom "pattern");
          (match years with [] -> None | _ -> Some Sexp.(List (Atom "years" :: years)));
          (match months with
           | [] -> None
           | _ -> Some Sexp.(List (Atom "months" :: months)));
          (match month_days with
           | [] -> None
           | _ -> Some Sexp.(List (Atom "month_days" :: month_days)));
          (match weekdays with
           | [] -> None
           | _ -> Some Sexp.(List (Atom "weekdays" :: weekdays)));
          (match hours with [] -> None | _ -> Some Sexp.(List (Atom "hours" :: hours)));
          (match minutes with
           | [] -> None
           | _ -> Some Sexp.(List (Atom "minutes" :: minutes)));
          (match seconds with
           | [] -> None
           | _ -> Some Sexp.(List (Atom "seconds" :: seconds)));
          (match ns with [] -> None | _ -> Some Sexp.(List (Atom "ns" :: ns)));
        ]
    )

let sexp_list_of_unary_op (op : Time_ast.unary_op) =
  match op with
  | Not -> [ Sexp.Atom "not" ]
  | Shift n -> [ Sexp.Atom "shift"; sexp_of_span n ]
  | Lengthen n -> [ Sexp.Atom "lengthen"; sexp_of_span n ]
  | With_tz tz ->
    [ Sexp.Atom "with_tz"; Sexp.Atom (Timedesc.Time_zone.name tz) ]

let to_sexp (t : Time_ast.t) : Sexp.t =
  let rec aux (t : Time_ast.t) =
    match t with
    | All -> Sexp.(List [ Atom "all" ])
    | Empty -> Sexp.(List [ Atom "empty" ])
    | Intervals s ->
      let l =
        s
        |> CCList.of_seq
        |> List.map (fun (x, y) ->
            Sexp.List [ sexp_of_timestamp x; sexp_of_timestamp y ])
      in
      Sexp.List (Sexp.Atom "intervals" :: l)
    | ISO_week_pattern (years, weeks) ->
      sexp_of_iso_week_pattern years weeks
    | Pattern pat -> sexp_of_pattern pat
    | Unary_op (op, t) -> Sexp.List (sexp_list_of_unary_op op @ [ aux t ])
    | Inter_seq s ->
      Sexp.(List (Atom "inter" :: (s |> Seq.map aux |> CCList.of_seq)))
    | Union_seq s ->
      Sexp.(List (Atom "union" :: (s |> Seq.map aux |> CCList.of_seq)))
    | Pattern_intervals { mode; bound; start; end_ } ->
      Sexp.(
        List
          [
            Atom "pattern_intervals";
            (match mode with
             | `Whole_inc -> Atom "whole_inc"
             | `Whole_exc -> Atom "whole_exc"
             | `Fst -> Atom "fst"
             | `Snd -> Atom "snd");
            sexp_of_span bound;
            sexp_of_points start;
            sexp_of_points end_;
          ])
    | Unchunk chunked -> Sexp.(List [ Atom "unchunk"; aux_chunked chunked ])
  and aux_chunked chunked =
    let sexp_list_of_unary_op_on_t op =
      match op with
      | Time_ast.Chunk_disjoint_interval -> [ Sexp.Atom "chunk_disjoint_intervals" ]
      | Chunk_at_year_boundary -> [ Sexp.Atom "chunk_at_year_boundary" ]
      | Chunk_at_month_boundary -> [ Sexp.Atom "chunk_at_month_boundary" ]
      | Chunk_by_duration { chunk_size; drop_partial } ->
        [
          Some (Sexp.Atom "chunk_by_duration");
          Some (sexp_of_span chunk_size);
          (if drop_partial then Some (Sexp.Atom "drop_partial") else None);
        ]
        |> CCList.filter_map CCFun.id
    in
    match chunked with
    | Unary_op_on_t (op, t) ->
      Sexp.(List (sexp_list_of_unary_op_on_t op @ [ aux t ]))
    | Unary_op_on_chunked (op, chunked) ->
      Sexp.(
        List
          (match op with
           | Drop n -> [ Atom "drop"; sexp_of_int n; aux_chunked chunked ]
           | Take n -> [ Atom "take"; sexp_of_int n; aux_chunked chunked ]
           | Take_nth n ->
             [ Atom "take_nth"; sexp_of_int n; aux_chunked chunked ]
           | Nth n -> [ Atom "nth"; sexp_of_int n; aux_chunked chunked ]
           | Chunk_again op ->
             [
               Atom "chunk_again";
               List (sexp_list_of_unary_op_on_t op @ [ aux_chunked chunked ]);
             ]))
  in
  aux t

let wrap_to_sexp_into_to_sexp_string (f : 'a -> Sexp.t) x =
  Sexp.to_string (f x)

let to_sexp_string = wrap_to_sexp_into_to_sexp_string to_sexp
