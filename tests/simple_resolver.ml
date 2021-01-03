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

let intervals_of_timestamps (s : Time.timestamp Seq.t) : Time.Interval.t Seq.t =
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

let timestamps_of_intervals (s : Time.Interval.t Seq.t) : Time.timestamp Seq.t =
  s |> Seq.flat_map (fun (a, b) -> Seq_utils.a_to_b_exc_int64 ~a ~b)

let normalize (s : Time.Interval.t Seq.t) : Time.Interval.t Seq.t =
  s
  |> timestamps_of_intervals
  |> Int64_set.of_seq
  |> Int64_set.to_seq
  |> intervals_of_timestamps

let find_after bound ((_start, end_exc) : Time.Interval.t)
    (s2 : Time.Interval.t Seq.t) =
  let s =
    s2
    |> OSeq.drop_while (fun (start', _) -> start' < end_exc)
    |> OSeq.take_while (fun (start', _) -> Int64.sub start' end_exc <= bound)
  in
  match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x

let do_chunk_at_year_boundary tz (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        Result.get_ok @@ Date_time.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time.of_timestamp ~tz_of_date_time:tz
        |> Result.get_ok
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time.set_to_last_day_hour_min_sec dt1
          |> Date_time.to_timestamp
          |> Date_time.max_of_timestamp_local_result
          |> Option.get
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
        Result.get_ok @@ Date_time.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time.of_timestamp ~tz_of_date_time:tz
        |> Result.get_ok
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time.set_to_last_day_hour_min_sec dt1
          |> Date_time.to_timestamp
          |> Date_time.max_of_timestamp_local_result
          |> Option.get
          |> Int64.succ
        in
        fun () ->
          Seq.Cons ((t1, t'), aux (fun () -> Seq.Cons ((t', t2), rest)))
  in
  aux s

let rec resolve ?(search_using_tz = Time_zone.utc)
    ~(search_start : Time.timestamp) ~(search_end_exc : Time.timestamp)
    (t : Time.t) : Time.Interval.t Seq.t =
  let open Time in
  let default_search_space =
    (default_search_space_start, default_search_space_end_exc)
  in
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
    | Timestamp_interval_seq (_, s) -> s
    | Round_robin_pick_list (_, l) ->
      l
      |> List.map (fun t -> aux search_space search_using_tz t)
      |> Time.Intervals.Round_robin
         .merge_multi_list_round_robin_non_decreasing ~skip_check:true
    | Unary_op (_, op, t) -> (
        match op with
        | Not ->
          Seq_utils.a_to_b_exc_int64 ~a:search_start ~b:search_end_exc
          |> Seq.filter (fun x ->
              Stdlib.not
                (mem ~search_start ~search_end_exc ~search_using_tz t x))
          |> intervals_of_timestamps
        | Every -> aux search_space search_using_tz t
        | Drop_n_points n ->
          aux default_search_space search_using_tz t
          |> timestamps_of_intervals
          |> OSeq.drop n
          |> intervals_of_timestamps
        | Take_n_points n ->
          aux default_search_space search_using_tz t
          |> timestamps_of_intervals
          |> OSeq.take n
          |> intervals_of_timestamps
        | Shift n ->
          let x, y = search_space in
          aux (Int64.sub x n, Int64.sub y n) search_using_tz t
          |> Seq.map (fun (x, y) -> (Int64.add n x, Int64.add n y))
        | Lengthen n ->
          let x, y = search_space in
          aux (x, Int64.add y n) search_using_tz t
          |> Seq.map (fun (x, y) -> (x, Int64.add n y))
        | With_tz tz -> aux search_space tz t)
    | After (_, b, t1, t2) ->
      let x, y = search_space in
      let search_space = (Int64.sub x b, y) in
      let s1 = aux search_space search_using_tz t1 in
      let s2 = aux search_space search_using_tz t2 in
      s1 |> Seq.filter_map (fun x -> find_after b x s2)
    | Between_inc (_, b, t1, t2) ->
      let x, y = search_space in
      let search_space = (Int64.sub x b, y) in
      let s1 = aux search_space search_using_tz t1 in
      let s2 = aux search_space search_using_tz t2 in
      s1
      |> Seq.filter_map (fun (start, end_exc) ->
          find_after b (start, end_exc) s2
          |> Option.map (fun (_, end_exc') -> (start, end_exc')))
    | Between_exc (_, b, t1, t2) ->
      let x, y = search_space in
      let search_space = (Int64.sub x b, y) in
      let s1 = aux search_space search_using_tz t1 in
      let s2 = aux search_space search_using_tz t2 in
      s1
      |> Seq.filter_map (fun (start, end_exc) ->
          find_after b (start, end_exc) s2
          |> Option.map (fun (start', _) -> (start, start')))
    | Unchunk (_, chunked) -> aux_chunked search_using_tz chunked |> normalize
    | _ ->
      Seq_utils.a_to_b_exc_int64 ~a:search_start ~b:search_end_exc
      |> Seq.filter (mem ~search_using_tz ~search_start ~search_end_exc t)
      |> intervals_of_timestamps
  and aux_chunked search_using_tz chunked =
    let chunk_based_on_op_on_t op s =
      match op with
      | Chunk_disjoint_interval -> normalize s
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

and mem ?(search_using_tz = Time_zone.utc) ~(search_start : Time.timestamp)
    ~(search_end_exc : Time.timestamp) (t : Time.t) (timestamp : Time.timestamp)
  : bool =
  let open Time in
  let rec aux t timestamp =
    match
      Time.Date_time.of_timestamp ~tz_of_date_time:search_using_tz timestamp
    with
    | Error () -> failwith (Printf.sprintf "Invalid timestamp: %Ld" timestamp)
    | Ok dt -> (
        let weekday =
          Result.get_ok
          @@ Time.weekday_of_month_day ~year:dt.year ~month:dt.month
            ~mday:dt.day
        in
        match t with
        | All -> true
        | Empty -> false
        | Timestamp_interval_seq (_, s) ->
          OSeq.exists
            (fun (start, end_exc) ->
               start <= timestamp && timestamp < end_exc)
            s
        | Pattern (_, pattern) ->
          let year_is_fine =
            Int_set.is_empty pattern.years
            || Int_set.mem dt.year pattern.years
          in
          let month_is_fine =
            Time.Month_set.is_empty pattern.months
            || Time.Month_set.mem dt.month pattern.months
          in
          let mday_is_fine =
            Int_set.is_empty pattern.month_days
            ||
            let day_count =
              day_count_of_month ~year:dt.year ~month:dt.month
            in
            pattern.month_days
            |> Int_set.to_seq
            |> Seq.map (fun mday ->
                if mday < 0 then day_count + mday + 1 else mday)
            |> OSeq.mem ~eq:( = ) dt.day
          in
          let wday_is_fine =
            Time.Weekday_set.is_empty pattern.weekdays
            || Time.Weekday_set.mem weekday pattern.weekdays
          in
          let hour_is_fine =
            Int_set.is_empty pattern.hours
            || Int_set.mem dt.hour pattern.hours
          in
          let minute_is_fine =
            Int_set.is_empty pattern.minutes
            || Int_set.mem dt.minute pattern.minutes
          in
          let second_is_fine =
            Int_set.is_empty pattern.seconds
            || Int_set.mem dt.second pattern.seconds
          in
          year_is_fine
          && month_is_fine
          && mday_is_fine
          && wday_is_fine
          && hour_is_fine
          && minute_is_fine
          && second_is_fine
        | Interval_inc (_, start, end_inc) ->
          start <= timestamp && timestamp <= end_inc
        | Interval_exc (_, start, end_inc) ->
          start <= timestamp && timestamp < end_inc
        | Unary_op (_, _, _)
        | Round_robin_pick_list (_, _)
        | After (_, _, _, _)
        | Between_inc (_, _, _, _)
        | Between_exc (_, _, _, _)
        | Unchunk _ ->
          resolve ~search_using_tz ~search_start ~search_end_exc t
          |> OSeq.exists (fun (x, y) -> x <= timestamp && timestamp < y)
        | Inter_seq (_, s) -> OSeq.for_all (fun t -> aux t timestamp) s
        | Union_seq (_, s) -> OSeq.exists (fun t -> aux t timestamp) s)
  in
  aux t timestamp
