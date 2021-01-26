open Date_time_components

let timestamp_safe_sub a b =
  if Int64.sub a Constants.timestamp_min >= b then Int64.sub a b
  else Constants.timestamp_min

let timestamp_safe_add a b =
  if Int64.sub Constants.timestamp_max a >= b then Int64.add a b
  else Constants.timestamp_max

let do_chunk ~drop_partial (n : int64) (s : Time.Interval.t Seq.t) :
  Time.Interval.t Seq.t =
  let rec aux n s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((x, y), rest) ->
      let size = Int64.sub y x in
      if size >= n then fun () ->
        Seq.Cons
          ( (x, Int64.add n x),
            aux n (fun () -> Seq.Cons ((Int64.add n x, y), rest)) )
      else if drop_partial then aux n rest
      else fun () -> Seq.Cons ((x, y), aux n rest)
  in
  aux n s

let intervals_of_timestamps (s : Time_ast.timestamp Seq.t) :
  Time.Interval.t Seq.t =
  let rec aux acc s =
    match s () with
    | Seq.Nil -> ( match acc with None -> Seq.empty | Some x -> Seq.return x)
    | Seq.Cons (x, rest) -> (
        match acc with
        | None -> aux (Some (x, Int64.succ x)) rest
        | Some (x', y') ->
          if y' = x then aux (Some (x', Int64.succ x)) rest
          else fun () -> Seq.Cons ((x', y'), aux None s))
  in
  aux None s

let timestamps_of_intervals (s : Time.Interval.t Seq.t) :
  Time_ast.timestamp Seq.t =
  s |> Seq.flat_map (fun (a, b) -> Seq_utils.a_to_b_exc_int64 ~a ~b)

let normalize (s : Time.Interval.t Seq.t) : Time.Interval.t Seq.t =
  s
  |> timestamps_of_intervals
  |> Int64_set.of_seq
  |> Int64_set.to_seq
  |> intervals_of_timestamps

let find_after bound (start : int64) (s2 : int64 Seq.t) =
  let s =
    s2
    |> OSeq.drop_while (fun start' -> start' <= start)
    |> OSeq.take_while (fun start' -> Int64.sub start' start <= bound)
  in
  match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x

let do_chunk_at_year_boundary tz (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        CCOpt.get_exn @@ Date_time'.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time'.of_timestamp ~tz_of_date_time:tz
        |> CCOpt.get_exn
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time'.set_to_last_day_hour_min_sec dt1
          |> Date_time'.to_timestamp
          |> Date_time'.max_of_timestamp_local_result
          |> Int64.succ
        in
        fun () ->
          Seq.Cons ((t1, t'), aux (fun () -> Seq.Cons ((t', t2), rest)))
  in
  aux s

let do_chunk_at_month_boundary tz (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        CCOpt.get_exn @@ Date_time'.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time'.of_timestamp ~tz_of_date_time:tz
        |> CCOpt.get_exn
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time'.set_to_last_day_hour_min_sec dt1
          |> Date_time'.to_timestamp
          |> Date_time'.max_of_timestamp_local_result
          |> Int64.succ
        in
        fun () ->
          Seq.Cons ((t1, t'), aux (fun () -> Seq.Cons ((t', t2), rest)))
  in
  aux s

let aux_pattern_mem search_using_tz (pattern : Pattern.t) timestamp =
  let dt =
    CCOpt.get_exn
    @@ Time.Date_time'.of_timestamp ~tz_of_date_time:search_using_tz timestamp
  in
  let weekday =
    CCOpt.get_exn
    @@ weekday_of_month_day ~year:dt.year ~month:dt.month ~mday:dt.day
  in
  let year_is_fine =
    Int_set.is_empty pattern.years || Int_set.mem dt.year pattern.years
  in
  let month_is_fine =
    Month_set.is_empty pattern.months || Month_set.mem dt.month pattern.months
  in
  let mday_is_fine =
    Int_set.is_empty pattern.month_days
    ||
    let day_count = day_count_of_month ~year:dt.year ~month:dt.month in
    pattern.month_days
    |> Int_set.to_seq
    |> Seq.map (fun mday -> if mday < 0 then day_count + mday + 1 else mday)
    |> OSeq.mem ~eq:( = ) dt.day
  in
  let wday_is_fine =
    Weekday_set.is_empty pattern.weekdays
    || Weekday_set.mem weekday pattern.weekdays
  in
  let hour_is_fine =
    Int_set.is_empty pattern.hours || Int_set.mem dt.hour pattern.hours
  in
  let minute_is_fine =
    Int_set.is_empty pattern.minutes || Int_set.mem dt.minute pattern.minutes
  in
  let second_is_fine =
    Int_set.is_empty pattern.seconds || Int_set.mem dt.second pattern.seconds
  in
  year_is_fine
  && month_is_fine
  && mday_is_fine
  && wday_is_fine
  && hour_is_fine
  && minute_is_fine
  && second_is_fine

let aux_pattern search_using_tz search_space pattern =
  Seq_utils.a_to_b_exc_int64 ~a:(fst search_space) ~b:(snd search_space)
  |> Seq.filter (aux_pattern_mem search_using_tz pattern)
  |> intervals_of_timestamps

let aux_points_mem search_using_tz ((p, tz_info) : Points.t) timestamp =
  let search_using_tz =
    match tz_info with
    | None -> search_using_tz
    | Some tz_info -> (
        match tz_info with
        | `Tz_only tz -> tz
        | `Tz_offset_s_only x -> Time_zone.make_offset_only x
        | `Tz_and_tz_offset_s (tz, _) -> tz)
  in
  aux_pattern_mem search_using_tz (Points.to_pattern (p, tz_info)) timestamp

let aux_points search_space search_using_tz points =
  Seq_utils.a_to_b_exc_int64 ~a:(fst search_space) ~b:(snd search_space)
  |> Seq.filter (aux_points_mem search_using_tz points)

let rec resolve ?(search_using_tz = Time_zone.utc)
    ~(search_start : Time_ast.timestamp) ~(search_end_exc : Time_ast.timestamp)
    (t : Time_ast.t) : Time.Interval.t Seq.t =
  let default_search_space = Time.(timestamp_min, timestamp_max) in
  let filter s =
    Seq.filter_map
      (fun (x, y) ->
         if y <= search_start then None
         else if search_end_exc < x then None
         else Some (max search_start x, min search_end_exc y))
      s
  in
  let rec aux (search_space : Time.Interval.t) (search_using_tz : Time_zone.t) t
    =
    match t with
    | Time_ast.Intervals s -> s
    | Unary_op (op, t) -> (
        match op with
        | Not ->
          Seq_utils.a_to_b_exc_int64 ~a:Time.timestamp_min
            ~b:Time.timestamp_max
          |> Seq.filter (fun x -> not (mem search_space ~search_using_tz t x))
          |> intervals_of_timestamps
        | Drop_points n ->
          aux default_search_space search_using_tz t
          |> timestamps_of_intervals
          |> OSeq.drop n
          |> intervals_of_timestamps
        | Take_points n ->
          aux default_search_space search_using_tz t
          |> timestamps_of_intervals
          |> OSeq.take n
          |> intervals_of_timestamps
        | Shift n ->
          let x, y = search_space in
          aux
            (timestamp_safe_sub x n, timestamp_safe_sub y n)
            search_using_tz t
          |> Seq.map (fun (x, y) ->
              (timestamp_safe_add n x, timestamp_safe_add n y))
        | Lengthen n ->
          let x, y = search_space in
          aux (x, Int64.add y n) search_using_tz t
          |> Seq.map (fun (x, y) -> (x, timestamp_safe_add n y))
        | With_tz tz -> aux search_space tz t)
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
              | `Snd -> (x, Int64.succ x)))
    | Unchunk chunked -> aux_chunked search_using_tz chunked |> normalize
    | _ ->
      Seq_utils.a_to_b_exc_int64 ~a:(fst search_space) ~b:(snd search_space)
      |> Seq.filter (mem ~search_using_tz search_space t)
      |> intervals_of_timestamps
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
      aux default_search_space search_using_tz t |> chunk_based_on_op_on_t op
    | Unary_op_on_chunked (op, c) -> (
        let s = aux_chunked search_using_tz c in
        match op with
        | Nth n -> s |> OSeq.drop n |> OSeq.take 1
        | Drop n -> OSeq.drop n s
        | Take n -> OSeq.take n s
        | Take_nth n -> OSeq.take_nth n s
        | Chunk_again op -> chunk_based_on_op_on_t op s)
  in
  aux (search_start, search_end_exc) search_using_tz t |> filter |> normalize

and mem ?(search_using_tz = Time_zone.utc)
    ((search_start, search_end_exc) : Time.Interval.t) (t : Time_ast.t)
    (timestamp : Time_ast.timestamp) : bool =
  let open Time_ast in
  let rec aux t timestamp =
    match t with
    | All -> true
    | Empty -> false
    | Intervals s ->
      OSeq.exists
        (fun (start, end_exc) -> start <= timestamp && timestamp < end_exc)
        s
    | Pattern pattern -> aux_pattern_mem search_using_tz pattern timestamp
    | Unary_op (_, _) | Bounded_intervals _ | Unchunk _ ->
      resolve ~search_using_tz ~search_start ~search_end_exc t
      |> OSeq.exists (fun (x, y) -> x <= timestamp && timestamp < y)
    | Inter_seq s -> OSeq.for_all (fun t -> aux t timestamp) s
    | Union_seq s -> OSeq.exists (fun t -> aux t timestamp) s
  in
  aux t timestamp
