open Span_set_utils

let timestamp_safe_sub a b =
  let open Timedesc.Span in
  if b >= zero then
    if a - Timedesc.Timestamp.min_val >= b then a - b
    else Timedesc.Timestamp.min_val
  else
    let b' = abs b in
    if Timedesc.Timestamp.max_val - a >= b' then a + b'
    else Timedesc.Timestamp.max_val

let timestamp_safe_add a b =
  let open Timedesc.Span in
  if b >= zero then
    if Timedesc.Timestamp.max_val - a >= b then a + b
    else Timedesc.Timestamp.max_val
  else
    let b' = abs b in
    if a - Timedesc.Timestamp.min_val >= b' then a - b'
    else Timedesc.Timestamp.min_val

let do_chunk ~drop_partial (n : Timedesc.Span.t) (s : Time.Interval'.t Seq.t) :
  Time.Interval'.t Seq.t =
  let open Timedesc.Span in
  let rec aux n s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((x, y), rest) ->
      let size = y - x in
      if size >= n then fun () ->
        Seq.Cons ((x, n + x), aux n (fun () -> Seq.Cons ((n + x, y), rest)))
      else if drop_partial then aux n rest
      else fun () -> Seq.Cons ((x, y), aux n rest)
  in
  aux n s

let normalize (s : Time.Interval'.t Seq.t) : Time.Interval'.t Seq.t =
  let set =
    Seq.fold_left
      (fun acc (x, y) -> Span_set.(add (Interval.make x y) acc))
      Span_set.empty s
  in
  Span_set.fold (fun i l -> Span_set.Interval.(x i, y i) :: l) set []
  |> List.rev
  |> CCList.to_seq

let find_after (bound : Timedesc.Span.t) (start : Timedesc.Span.t)
    (s2 : Timedesc.Span.t Seq.t) : Timedesc.Span.t option =
  let open Timedesc.Span in
  let s =
    s2
    |> OSeq.drop_while (fun start' -> start' <= start)
    |> OSeq.take_while (fun start' -> start' - start <= bound)
  in
  match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x

let do_chunk_at_year_boundary tz (s : Time.Interval'.t Seq.t) :
  Time.Interval'.t Seq.t =
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        CCOpt.get_exn_or "Expected successful date time construction"
        @@ Timedesc.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Timedesc.Span.pred
        |> Timedesc.of_timestamp ~tz_of_date_time:tz
        |> CCOpt.get_exn_or "Expected successful date time construction"
      in
      if Timedesc.year dt1 = Timedesc.year dt2 then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Timedesc.make_exn ~tz:(Timedesc.tz dt1) ~year:(Timedesc.year dt1)
            ~month:12 ~day:31 ~hour:23 ~minute:59 ~second:59
            ~ns:(Timedesc.Span.ns_count_in_s - 1)
            ()
          |> Timedesc.to_timestamp
          |> Timedesc.max_of_local_result
          |> Timedesc.Span.succ
        in
        fun () ->
          Seq.Cons ((t1, t'), aux (fun () -> Seq.Cons ((t', t2), rest)))
  in
  aux s

let do_chunk_at_month_boundary tz (s : Time.Interval'.t Seq.t) :
  Time.Interval'.t Seq.t =
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        CCOpt.get_exn_or "Expected successful date time construction"
        @@ Timedesc.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Timedesc.Span.pred
        |> Timedesc.of_timestamp ~tz_of_date_time:tz
        |> CCOpt.get_exn_or "Expected successful date time construction"
      in
      if
        Timedesc.year dt1 = Timedesc.year dt2
        && Timedesc.month dt1 = Timedesc.month dt2
      then fun () -> Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Timedesc.make_exn ~tz:(Timedesc.tz dt1) ~year:(Timedesc.year dt1)
            ~month:(Timedesc.month dt1)
            ~day:
              (Timedesc.Utils.day_count_of_month ~year:(Timedesc.year dt1)
                 ~month:(Timedesc.month dt1))
            ~hour:23 ~minute:59 ~second:59
            ~ns:(Timedesc.Span.ns_count_in_s - 1)
            ()
          |> Timedesc.to_timestamp
          |> Timedesc.max_of_local_result
          |> Timedesc.Span.succ
        in
        fun () ->
          Seq.Cons ((t1, t'), aux (fun () -> Seq.Cons ((t', t2), rest)))
  in
  aux s

let aux_pattern_mem search_using_tz (pattern : Pattern.t) (timestamp : int64) :
  bool =
  let dt =
    CCOpt.get_exn_or "Expected successful date time construction"
    @@ Timedesc.of_timestamp ~tz_of_date_time:search_using_tz
      (Timedesc.Span.make ~s:timestamp ())
  in
  let weekday = Timedesc.weekday dt in
  let year_is_fine =
    Int_set.is_empty pattern.years
    || Int_set.mem (Timedesc.year dt) pattern.years
  in
  let month_is_fine =
    Int_set.is_empty pattern.months
    || Int_set.mem (Timedesc.month dt) pattern.months
  in
  let mday_is_fine =
    Int_set.is_empty pattern.month_days
    ||
    let day_count =
      Timedesc.Utils.day_count_of_month ~year:(Timedesc.year dt)
        ~month:(Timedesc.month dt)
    in
    pattern.month_days
    |> Int_set.to_seq
    |> Seq.map (fun mday -> if mday < 0 then day_count + mday + 1 else mday)
    |> OSeq.mem ~eq:( = ) (Timedesc.day dt)
  in
  let wday_is_fine =
    Weekday_set.is_empty pattern.weekdays
    || Weekday_set.mem weekday pattern.weekdays
  in
  let hour_is_fine =
    Int_set.is_empty pattern.hours
    || Int_set.mem (Timedesc.hour dt) pattern.hours
  in
  let minute_is_fine =
    Int_set.is_empty pattern.minutes
    || Int_set.mem (Timedesc.minute dt) pattern.minutes
  in
  let second_is_fine =
    Int_set.is_empty pattern.seconds
    || Int_set.mem (Timedesc.second dt) pattern.seconds
  in
  year_is_fine
  && month_is_fine
  && mday_is_fine
  && wday_is_fine
  && hour_is_fine
  && minute_is_fine
  && second_is_fine

let aux_pattern (search_start, search_end_exc) search_using_tz pattern :
  Span_set.t =
  let search_space_set =
    span_set_of_intervals @@ Seq.return (search_start, search_end_exc)
  in
  Seq_utils.a_to_b_inc_int64
    ~a:Timedesc.Span.(search_start.s)
    ~b:Timedesc.Span.(search_end_exc.s)
  |> Seq.filter (aux_pattern_mem search_using_tz pattern)
  |> intervals_of_int64s
  |> span_set_of_intervals
  |> Span_set.inter search_space_set

let aux_points_mem search_using_tz ({ pick; tz_info } : Points.t) timestamp =
  let search_using_tz =
    match tz_info with
    | None -> search_using_tz
    | Some Timedesc.Time_zone_info.{ tz; _ } -> tz
  in
  aux_pattern_mem search_using_tz
    (Points.to_pattern { pick; tz_info })
    timestamp

let aux_points (search_start, search_end_exc) search_using_tz points :
  Timedesc.Span.t Seq.t =
  Seq_utils.a_to_b_inc_int64
    ~a:Timedesc.Span.(search_start.s)
    ~b:Timedesc.Span.(search_end_exc.s)
  |> Seq.filter (aux_points_mem search_using_tz points)
  |> intervals_of_int64s
  |> Seq.map fst

let resolve ?(search_using_tz = Timedesc.Time_zone.utc)
    ~(search_start : Timedesc.Span.t) ~(search_end_exc : Timedesc.Span.t)
    (t : Time_ast.t) : Time.Interval'.t Seq.t =
  let default_search_space = Timedesc.(Timestamp.min_val, Timestamp.max_val) in
  let rec aux (search_space : Time.Interval'.t)
      (search_using_tz : Timedesc.Time_zone.t) t =
    match t with
    | Time_ast.Empty -> Span_set.empty
    | All -> span_set_full
    | Intervals s -> span_set_of_intervals s
    | Pattern p -> aux_pattern search_space search_using_tz p
    | Unary_op (op, t) -> (
        match op with
        | Not ->
          Span_set.diff span_set_full (aux search_space search_using_tz t)
        | Shift n ->
          let x, y = search_space in
          aux
            (timestamp_safe_sub x n, timestamp_safe_sub y n)
            search_using_tz t
          |> span_set_map (fun (x, y) ->
              (timestamp_safe_add n x, timestamp_safe_add n y))
        | Lengthen n ->
          let x, y = search_space in
          aux (x, timestamp_safe_add y n) search_using_tz t
          |> span_set_map (fun (x, y) -> (x, timestamp_safe_add n y))
        | With_tz tz -> aux search_space tz t)
    | Inter_seq s ->
      Seq.fold_left
        (fun acc t -> Span_set.inter acc (aux search_space search_using_tz t))
        span_set_full s
    | Union_seq s ->
      Seq.fold_left
        (fun acc t -> Span_set.union acc (aux search_space search_using_tz t))
        Span_set.empty s
    | Bounded_intervals { pick; bound; start; end_exc } ->
      let x, y = search_space in
      let search_space = (timestamp_safe_sub x bound, y) in
      let s1 = aux_points search_space search_using_tz start in
      let s2 = aux_points search_space search_using_tz end_exc in
      s1
      |> Seq.filter_map (fun start ->
          find_after bound start s2
          |> CCOpt.map (fun x ->
              match pick with
              | `Whole -> (start, x)
              | `Snd -> (x, Timedesc.Span.succ x)))
      |> span_set_of_intervals
    | Unchunk chunked ->
      aux_chunked search_using_tz chunked |> span_set_of_intervals
  and aux_chunked search_using_tz chunked =
    let chunk_based_on_op_on_t op s =
      match op with
      | Time_ast.Chunk_disjoint_interval -> normalize s
      | Chunk_by_duration { chunk_size; drop_partial } ->
        do_chunk ~drop_partial chunk_size s
      | Chunk_at_year_boundary -> do_chunk_at_year_boundary search_using_tz s
      | Chunk_at_month_boundary -> do_chunk_at_month_boundary search_using_tz s
    in
    match chunked with
    | Unary_op_on_t (op, t) ->
      aux default_search_space search_using_tz t
      |> intervals_of_span_set
      |> chunk_based_on_op_on_t op
    | Unary_op_on_chunked (op, c) -> (
        let s = aux_chunked search_using_tz c in
        match op with
        | Nth n -> s |> OSeq.drop n |> OSeq.take 1
        | Drop n -> OSeq.drop n s
        | Take n -> OSeq.take n s
        | Take_nth n -> OSeq.take_nth n s
        | Chunk_again op -> chunk_based_on_op_on_t op s)
  in
  let search_space_set =
    span_set_of_intervals @@ Seq.return (search_start, search_end_exc)
  in
  aux (search_start, search_end_exc) search_using_tz t
  |> Span_set.inter search_space_set
  |> intervals_of_span_set
