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
    | Seq.Nil -> ( match acc with None -> Seq.empty | Some x -> Seq.return x )
    | Seq.Cons (x, rest) -> (
        match acc with
        | None -> aux (Some (x, Int64.succ x)) rest
        | Some (x', y') ->
          if y' = x then aux (Some (x', Int64.succ x)) rest
          else fun () -> Seq.Cons ((x', y'), aux None s) )
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

let find_after ((_start, end_exc) : Time.Interval.t)
    (s2 : Time.Interval.t Seq.t) =
  match OSeq.drop_while (fun (start', _) -> start' < end_exc) s2 () with
  | Seq.Nil -> None
  | Seq.Cons (x, _) -> Some x

let do_chunk_at_year_boundary tz_offset_s (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        Result.get_ok
        @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s t1
      in
      let dt2 =
        Result.get_ok
        @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s t2
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time.set_to_last_day_hour_min_sec dt1
          |> Date_time.to_timestamp
          |> Int64.succ
        in
        OSeq.cons (t1, t') (aux (OSeq.cons (t', t2) rest))
  in
  aux s

let do_chunk_at_month_boundary tz_offset_s (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        Result.get_ok
        @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s t1
      in
      let dt2 =
        Result.get_ok
        @@ Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s t2
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time.set_to_last_day_hour_min_sec dt1
          |> Date_time.to_timestamp
          |> Int64.succ
        in
        OSeq.cons (t1, t') (aux (OSeq.cons (t', t2) rest))
  in
  aux s

let rec resolve ?(search_using_tz_offset_s = 0) ~(search_start : Time.timestamp)
    ~(search_end_exc : Time.timestamp) (t : Time.t) : Time.Interval.t Seq.t =
  let open Time in
  let filter s =
    Seq.filter_map
      (fun (x, y) ->
         if y <= search_start then None
         else if search_end_exc < x then None
         else Some (max search_start x, min search_end_exc y))
      s
  in
  let rec aux (search_using_tz_offset_s : Time.tz_offset_s) t =
    match t with
    | Timestamp_interval_seq (_, s) -> s
    | Round_robin_pick_list (_, l) ->
      l
      |> List.map (fun t -> aux search_using_tz_offset_s t)
      |> Time.Intervals.Round_robin
         .merge_multi_list_round_robin_non_decreasing ~skip_check:true
    | Unary_op (_, op, t) -> (
        match op with
        | Not ->
          Seq_utils.a_to_b_exc_int64 ~a:search_start ~b:search_end_exc
          |> Seq.filter (fun x ->
              Stdlib.not
                (mem ~search_start ~search_end_exc
                   ~search_using_tz_offset_s t x))
          |> intervals_of_timestamps
        | Every -> aux search_using_tz_offset_s t
        | Skip_n_points n ->
          aux search_using_tz_offset_s t
          |> timestamps_of_intervals
          |> OSeq.drop n
          |> intervals_of_timestamps
        | Take_n_points n ->
          aux search_using_tz_offset_s t
          |> timestamps_of_intervals
          |> OSeq.take n
          |> intervals_of_timestamps
        | Shift n ->
          aux search_using_tz_offset_s t
          |> Seq.map (fun (x, y) -> (Int64.add n x, Int64.add n y))
        | Lengthen n ->
          aux search_using_tz_offset_s t
          |> Seq.map (fun (x, y) -> (x, Int64.add n y))
        | Change_tz_offset_s n -> aux n t )
    | After (_, t1, t2) ->
      let s1 = aux search_using_tz_offset_s t1 in
      let s2 = aux search_using_tz_offset_s t2 in
      s1 |> Seq.filter_map (fun x -> find_after x s2)
    | Between_inc (_, t1, t2) ->
      let s1 = aux search_using_tz_offset_s t1 in
      let s2 = aux search_using_tz_offset_s t2 in
      s1
      |> Seq.filter_map (fun (start, end_exc) ->
          find_after (start, end_exc) s2
          |> Option.map (fun (_, end_exc') -> (start, end_exc')))
    | Between_exc (_, t1, t2) ->
      let s1 = aux search_using_tz_offset_s t1 in
      let s2 = aux search_using_tz_offset_s t2 in
      s1
      |> Seq.filter_map (fun (start, end_exc) ->
          find_after (start, end_exc) s2
          |> Option.map (fun (start', _) -> (start, start')))
    | Unchunk chunked ->
      aux_chunked search_using_tz_offset_s chunked |> normalize
    | _ ->
      Seq_utils.a_to_b_exc_int64 ~a:search_start ~b:search_end_exc
      |> Seq.filter
        (mem ~search_using_tz_offset_s ~search_start ~search_end_exc t)
      |> intervals_of_timestamps
  and aux_chunked search_using_tz_offset_s chunked =
    let chunk_based_on_op_on_t op s =
      match op with
      | Chunk_as_is -> s
      | Chunk_by_duration { chunk_size; drop_partial } ->
        do_chunk ~drop_partial chunk_size s
      | Chunk_at_year_boundary ->
        do_chunk_at_year_boundary search_using_tz_offset_s s
      | Chunk_at_month_boundary ->
        do_chunk_at_month_boundary search_using_tz_offset_s s
    in
    match chunked with
    | Unary_op_on_t (op, t) ->
      aux search_using_tz_offset_s t |> chunk_based_on_op_on_t op
    | Unary_op_on_chunked (op, c) -> (
        let s = aux_chunked search_using_tz_offset_s c in
        match op with
        | Nth n -> s |> OSeq.drop n |> OSeq.take 1
        | Skip_n n -> OSeq.drop n s
        | Take_n n -> OSeq.take n s
        | Every_nth n -> OSeq.take_nth n s
        | Chunk_again op -> chunk_based_on_op_on_t op s )
  in
  aux search_using_tz_offset_s t |> filter |> normalize

and mem ?(search_using_tz_offset_s = 0) ~(search_start : Time.timestamp)
    ~(search_end_exc : Time.timestamp) (t : Time.t) (timestamp : Time.timestamp)
  : bool =
  let open Time in
  let rec aux t timestamp =
    match
      Time.Date_time.of_timestamp
        ~tz_offset_s_of_date_time:search_using_tz_offset_s timestamp
    with
    | Error () -> failwith (Printf.sprintf "Invalid timestamp: %Ld" timestamp)
    | Ok dt -> (
        let weekday =
          Result.get_ok
          @@ Time.weekday_of_month_day ~year:dt.year ~month:dt.month
            ~mday:dt.day
        in
        let second_of_day =
          make_hms ~hour:dt.hour ~minute:dt.minute ~second:dt.second
          |> Result.get_ok
          |> second_of_day_of_hms
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
        | Branching (_, branching) ->
          let years =
            branching.years
            |> List.to_seq
            |> Seq.flat_map (fun year_range ->
                match year_range with
                | `Range_inc (x, y) -> OSeq.(x -- y)
                | `Range_exc (x, y) -> OSeq.(x --^ y))
          in
          let year_months =
            years
            |> Seq.flat_map (fun year ->
                branching.months
                |> List.to_seq
                |> Seq.flat_map (fun month_range ->
                    match month_range with
                    | `Range_inc (x, y) ->
                      let x = tm_int_of_month x in
                      let y = tm_int_of_month y in
                      OSeq.(x -- y)
                      |> Seq.map (fun x ->
                          Result.get_ok @@ month_of_tm_int x)
                      |> Seq.map (fun month -> (year, month))
                    | `Range_exc (x, y) ->
                      let x = tm_int_of_month x in
                      let y = tm_int_of_month y in
                      OSeq.(x --^ y)
                      |> Seq.map (fun x ->
                          Result.get_ok @@ month_of_tm_int x)
                      |> Seq.map (fun month -> (year, month))))
          in
          ( match branching.days with
            | Month_days days ->
              let year_month_days =
                year_months
                |> Seq.flat_map (fun (year, month) ->
                    let day_count_of_month =
                      day_count_of_month ~year ~month
                    in
                    days
                    |> List.to_seq
                    |> OSeq.flat_map (fun day_range ->
                        match day_range with
                        | `Range_inc (x, y) ->
                          let x =
                            if x < 0 then day_count_of_month + x + 1
                            else x
                          in
                          let y =
                            if y < 0 then day_count_of_month + y + 1
                            else y
                          in
                          OSeq.(x -- y)
                          |> Seq.map (fun x -> (year, month, x))
                        | `Range_exc (x, y) ->
                          let x =
                            if x < 0 then day_count_of_month + x + 1
                            else x
                          in
                          let y =
                            if y < 0 then day_count_of_month + y + 1
                            else y
                          in
                          OSeq.(x --^ y)
                          |> Seq.map (fun x -> (year, month, x))))
              in
              OSeq.exists
                (fun (year, month, day) ->
                   dt.year = year && dt.month = month && dt.day = day)
                year_month_days
            | Weekdays days ->
              List.mem weekday (Weekday_ranges.Flatten.flatten_list days) )
          && List.exists
            (fun hmss_range ->
               match hmss_range with
               | `Range_inc (x, y) ->
                 let x = Time.second_of_day_of_hms x in
                 let y = Time.second_of_day_of_hms y in
                 x <= second_of_day && second_of_day <= y
               | `Range_exc (x, y) ->
                 let x = Time.second_of_day_of_hms x in
                 let y = Time.second_of_day_of_hms y in
                 x <= second_of_day && second_of_day < y)
            branching.hmss
        | Recur (_, recur) -> failwith "Unimplemented"
        | Interval_inc (_, start, end_inc) ->
          start <= timestamp && timestamp <= end_inc
        | Interval_exc (_, start, end_inc) ->
          start <= timestamp && timestamp < end_inc
        | Unary_op (_, _, _)
        | Round_robin_pick_list (_, _)
        | After (_, _, _)
        | Between_inc (_, _, _)
        | Between_exc (_, _, _)
        | Unchunk _ ->
          resolve ~search_using_tz_offset_s ~search_start ~search_end_exc t
          |> OSeq.exists (fun (x, y) -> x <= timestamp && timestamp < y)
        | Inter_seq (_, s) -> OSeq.for_all (fun t -> aux t timestamp) s
        | Union_seq (_, s) -> OSeq.exists (fun t -> aux t timestamp) s )
  in
  aux t timestamp
