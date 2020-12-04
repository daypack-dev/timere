let do_skip_n_points (n : int64) (s : Time.Interval.t Seq.t) :
  Time.Interval.t Seq.t =
  let rec aux n s =
    if n = 0L then s
    else
      match s () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((x, y), rest) ->
        let size = Int64.sub y x in
        if size >= n then fun () -> Seq.Cons ((Int64.add n x, y), rest)
        else aux (Int64.sub n size) rest
  in
  aux n s

let do_take_n_points (n : int64) (s : Time.Interval.t Seq.t) :
  Time.Interval.t Seq.t =
  let rec aux n s =
    if n = 0L then Seq.empty
    else
      match s () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((x, y), rest) ->
        let size = Int64.sub y x in
        if size >= n then Seq.return (x, Int64.add n x)
        else fun () -> Seq.Cons ((x, y), aux (Int64.sub n size) rest)
  in
  aux n s

let do_chunk (n : int64) drop_partial (s : Time.Interval.t Seq.t) :
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
      else fun () -> Seq.Cons ((x, Int64.add n x), aux n rest)
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
          else fun () -> Seq.Cons ((x', y'), aux None rest) )
  in
  aux None s

let rec resolve ~(search_start : Time.timestamp)
    ~(search_end_exc : Time.timestamp) ~tz_offset_s (t : Time.t) :
  Time.Interval.t Seq.t =
  let open Time in
  let filter s =
    Seq.filter_map (fun (x, y) ->
        if y <= search_start then
          None
        else
          if search_end_exc < x then
            None
          else
            Some (max search_start x,
                  min search_end_exc y
                 )
      ) s
  in
  let rec aux t cur end_exc tz_offset_s =
    match t with
    | Timestamp_interval_seq (_, s) -> s
    | Round_robin_pick_list (_, l) ->
      l
      |> List.map (fun t -> aux t cur end_exc tz_offset_s)
      |> Time.Intervals.Round_robin
         .merge_multi_list_round_robin_non_decreasing ~skip_check:true
    | Unary_op (_, op, t) -> (
        match op with
        | Not ->
          Seq_utils.a_to_b_exc_int64 ~a:cur ~b:end_exc
          |> Seq.filter (fun x ->
              Stdlib.not
                (mem ~search_start ~search_end_exc ~tz_offset_s t x))
          |> intervals_of_timestamps
        | Every -> aux t cur end_exc tz_offset_s
        | Skip_n_points n ->
          do_skip_n_points (Int64.of_int n) (aux t cur end_exc tz_offset_s)
        | Skip_n_intervals n -> OSeq.drop n (aux t cur end_exc tz_offset_s)
        | Next_n_points n ->
          do_take_n_points (Int64.of_int n) (aux t cur end_exc tz_offset_s)
        | Next_n_intervals n -> OSeq.take n (aux t cur end_exc tz_offset_s)
        | Chunk { chunk_size; drop_partial } ->
          do_chunk chunk_size drop_partial (aux t cur end_exc tz_offset_s)
        | Shift n ->
          aux t cur end_exc tz_offset_s
          |> Seq.map (fun (x, y) -> (Int64.add n x, Int64.add n y))
        | Lengthen n ->
          aux t cur end_exc tz_offset_s
          |> Seq.map (fun (x, y) -> (Int64.add n x, Int64.add n y))
        | Tz_offset_s n -> aux t cur end_exc n )
    | _ ->
      Seq_utils.a_to_b_exc_int64 ~a:cur ~b:end_exc
      |> Seq.filter (mem ~search_start ~search_end_exc ~tz_offset_s t)
      |> intervals_of_timestamps
  in
  aux t search_start search_end_exc tz_offset_s
  |> filter
  |> Time.Intervals.Normalize.normalize ~skip_filter_invalid:true ~skip_filter_empty:true ~skip_sort:true

and mem ~(search_start : Time.timestamp) ~(search_end_exc : Time.timestamp)
    ~tz_offset_s (t : Time.t) (timestamp : Time.timestamp) : bool =
  let open Time in
  let rec aux t timestamp =
    match
      Time.Date_time.of_timestamp ~tz_offset_s_of_date_time:tz_offset_s
        timestamp
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
        | Timestamp_interval_seq (_, s) ->
          OSeq.exists
            (fun (start, end_exc) ->
               start <= timestamp && timestamp < end_exc)
            s
        | Pattern (_, pattern) ->
          let year_is_fine =
            match pattern.years with [] -> true | l -> List.mem dt.year l
          in
          let month_is_fine =
            match pattern.months with [] -> true | l -> List.mem dt.month l
          in
          let mday_is_fine =
            match pattern.month_days with
            | [] -> true
            | l -> List.mem dt.day l
          in
          let wday_is_fine =
            match pattern.weekdays with [] -> true | l -> List.mem weekday l
          in
          let hour_is_fine =
            match pattern.hours with [] -> true | l -> List.mem dt.hour l
          in
          let minute_is_fine =
            match pattern.hours with [] -> true | l -> List.mem dt.minute l
          in
          let second_is_fine =
            match pattern.hours with [] -> true | l -> List.mem dt.second l
          in
          let timestamp_is_fine =
            match pattern.timestamps with
            | [] -> true
            | l -> List.mem timestamp l
          in
          year_is_fine
          && month_is_fine
          && mday_is_fine
          && wday_is_fine
          && hour_is_fine
          && minute_is_fine
          && second_is_fine
          && timestamp_is_fine
        | Branching (_, branching) ->
          List.exists
            (fun year_range ->
               match year_range with
               | `Range_inc (x, y) -> x <= dt.year && dt.year <= y
               | `Range_exc (x, y) -> x <= dt.year && dt.year < y)
            branching.years
          && List.exists
            (fun month_range ->
               match month_range with
               | `Range_inc (x, y) ->
                 month_le x dt.month && month_le dt.month y
               | `Range_exc (x, y) ->
                 month_le x dt.month && month_lt dt.month y)
            branching.months
          && ( match branching.days with
              | Month_days days ->
                List.exists
                  (fun day_range ->
                     match day_range with
                     | `Range_inc (x, y) -> x <= dt.day && dt.day <= y
                     | `Range_exc (x, y) -> x <= dt.day && dt.day < y)
                  days
              | Weekdays days ->
                List.mem weekday (Weekday_ranges.Flatten.flatten_list days)
            )
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
        | Interval_inc (_, start, end_inc) ->
          start <= timestamp && timestamp <= end_inc
        | Interval_exc (_, start, end_inc) ->
          start <= timestamp && timestamp < end_inc
        | Unary_op (_, _, _) | Round_robin_pick_list (_, _) ->
          resolve ~search_start ~search_end_exc ~tz_offset_s t
          |> OSeq.exists (fun (x, y) -> x <= timestamp && timestamp < y)
        | Inter_list (_, l) -> List.for_all (fun t -> aux t timestamp) l
        | Union_list (_, l) -> List.exists (fun t -> aux t timestamp) l )
  in
  aux t timestamp
