open Int64_utils
open Time_ast

type search_space = Time.Interval.t list

let default_search_space_start = Time.min_timestamp

let default_search_space_end_exc = Time.max_timestamp

let default_search_space : search_space =
  [ (default_search_space_start, default_search_space_end_exc) ]

type t =
  | Empty
  | All
  | Timestamp_interval_seq of search_space * (int64 * int64) Seq.t
  | Pattern of search_space * Pattern.t
  | Unary_op of search_space * unary_op * t
  | Interval_inc of search_space * timestamp * timestamp
  | Interval_exc of search_space * timestamp * timestamp
  | Round_robin_pick_list of search_space * t list
  | Inter_seq of search_space * t Seq.t
  | Union_seq of search_space * t Seq.t
  | After of search_space * int64 * t * t
  | Between_inc of search_space * int64 * t * t
  | Between_exc of search_space * int64 * t * t
  | Unchunk of search_space * chunked

and chunked =
  | Unary_op_on_t of chunked_unary_op_on_t * t
  | Unary_op_on_chunked of chunked_unary_op_on_chunked * chunked

let rec t_of_ast (ast : Time_ast.t) : t =
  match ast with
  | Empty -> Empty
  | All -> All
  | Timestamp_interval_seq s ->
    Timestamp_interval_seq (default_search_space, s)
  | Pattern p ->
    Pattern (default_search_space, p)
  | Unary_op (op, t) ->
    Unary_op (default_search_space, op, t_of_ast t)
  | Interval_inc (t1, t2) ->
    Interval_inc (default_search_space, t1, t2)
  | Interval_exc (t1, t2) ->
    Interval_exc (default_search_space, t1, t2)
  | Round_robin_pick_list l ->
    Round_robin_pick_list (default_search_space, List.map t_of_ast l)
  | Inter_seq s ->
    Inter_seq (default_search_space, Seq.map t_of_ast s)
  | Union_seq s ->
    Union_seq (default_search_space, Seq.map t_of_ast s)
  | After (bound, t1, t2) ->
    After (default_search_space, bound, t_of_ast t1, t_of_ast t2)
  | Between_inc (bound, t1, t2) ->
    Between_inc (default_search_space, bound, t_of_ast t1, t_of_ast t2)
  | Between_exc (bound, t1, t2) ->
    Between_exc (default_search_space, bound, t_of_ast t1, t_of_ast t2)
  | Unchunk chunked ->
    Unchunk (default_search_space, chunked_of_ast_chunked chunked)

and chunked_of_ast_chunked (c : Time_ast.chunked) : chunked =
  match c with
  | Unary_op_on_t (op, t) ->
    Unary_op_on_t (op, t_of_ast t)
  | Unary_op_on_chunked (op, chunked) ->
    Unary_op_on_chunked (op, chunked_of_ast_chunked chunked)

let get_search_space (time : t) : Time.Interval.t list =
  match time with
  | All -> default_search_space
  | Empty -> []
  | Timestamp_interval_seq (s, _) -> s
  | Pattern (s, _) -> s
  | Unary_op (s, _, _) -> s
  | Interval_exc (s, _, _) -> s
  | Interval_inc (s, _, _) -> s
  | Round_robin_pick_list (s, _) -> s
  | Inter_seq (s, _) -> s
  | Union_seq (s, _) -> s
  | After (s, _, _, _) -> s
  | Between_inc (s, _, _, _) -> s
  | Between_exc (s, _, _, _) -> s
  | Unchunk (s, _) -> s

let calibrate_search_space (time : t) space : search_space =
  match time with
  | All | Empty | Timestamp_interval_seq _ | Pattern _ -> space
  | Unary_op (_, op, _) -> (
      match op with
      | Shift n -> List.map (fun (x, y) -> (Int64.sub x n, Int64.sub y n)) space
      | _ -> space)
  | Interval_exc _ | Interval_inc _ | Round_robin_pick_list _ | Inter_seq _
  | Union_seq _ ->
    space
  | After (_, b, _, _) -> (
      match space with [] -> [] | (x, y) :: rest -> (Int64.sub x b, y) :: rest)
  | Between_inc (_, b, _, _) -> (
      match space with [] -> [] | (x, y) :: rest -> (Int64.sub x b, y) :: rest)
  | Between_exc (_, b, _, _) -> (
      match space with [] -> [] | (x, y) :: rest -> (Int64.sub x b, y) :: rest)
  | Unchunk _ -> space

let set_search_space space (time : t) : t =
  match time with
  | All -> All
  | Empty -> Empty
  | Timestamp_interval_seq (_, x) -> Timestamp_interval_seq (space, x)
  | Pattern (_, x) -> Pattern (space, x)
  | Unary_op (_, op, x) -> Unary_op (space, op, x)
  | Interval_exc (_, x, y) -> Interval_exc (space, x, y)
  | Interval_inc (_, x, y) -> Interval_inc (space, x, y)
  | Round_robin_pick_list (_, x) -> Round_robin_pick_list (space, x)
  | Inter_seq (_, x) -> Inter_seq (space, x)
  | Union_seq (_, x) -> Union_seq (space, x)
  | After (_, b, x, y) -> After (space, b, x, y)
  | Between_inc (_, b, x, y) -> Between_inc (space, b, x, y)
  | Between_exc (_, b, x, y) -> Between_exc (space, b, x, y)
  | Unchunk (_, c) -> Unchunk (space, c)

let search_space_of_year_range tz year_range =
  let open Time in
  let aux_start start =
    Date_time'.set_to_first_month_day_hour_min_sec
      { Date_time'.min with year = start; tz_info = `Tz_only tz }
    |> Date_time'.to_timestamp
    |> Date_time'.min_of_timestamp_local_result
    |> CCOpt.get_exn
  in
  let aux_end_inc end_exc =
    Date_time'.set_to_last_month_day_hour_min_sec
      { Date_time'.min with year = end_exc; tz_info = `Tz_only tz }
    |> Date_time'.to_timestamp
    |> Date_time'.min_of_timestamp_local_result
    |> CCOpt.get_exn
    |> Int64.succ
  in
  let aux_end_exc end_exc =
    Date_time'.set_to_first_month_day_hour_min_sec
      { Date_time'.min with year = end_exc; tz_info = `Tz_only tz }
    |> Date_time'.to_timestamp
    |> Date_time'.min_of_timestamp_local_result
    |> CCOpt.get_exn
  in
  match year_range with
  | `Range_inc (start, end_inc) -> (aux_start start, aux_end_inc end_inc)
  | `Range_exc (start, end_exc) -> (aux_start start, aux_end_exc end_exc)

let search_space_of_year tz_offset_s year =
  search_space_of_year_range tz_offset_s (`Range_inc (year, year))

let empty_search_space = []

let propagate_search_space_bottom_up default_tz (time : t) : t =
  let open Time in
  let rec aux (tz : Time_zone.t) (time : t) : t =
    match time with
    | All -> All
    | Empty -> Empty
    | Timestamp_interval_seq (_, s) -> (
        match s () with
        | Seq.Nil -> time
        | Seq.Cons ((start, _), _) ->
          Timestamp_interval_seq ([ (start, default_search_space_end_exc) ], s)
      )
    | Pattern (_, pat) ->
      if Int_set.is_empty pat.years then Pattern (default_search_space, pat)
      else
        let space =
          pat.years
          |> Int_set.to_seq
          |> Seq.map (search_space_of_year tz)
          |> CCList.of_seq
        in
        Pattern (space, pat)
    | Unary_op (_, op, t) -> (
        match op with
        | Not -> Unary_op (default_search_space, op, aux tz t)
        | With_tz tz ->
          let t = aux tz t in
          Unary_op (get_search_space t, op, t)
        | _ ->
          let t = aux tz t in
          Unary_op (get_search_space t, op, t))
    | Inter_seq (_, s) ->
      let s = Seq.map (aux tz) s in
      let space =
        s
        |> Seq.map get_search_space
        |> Seq.map CCList.to_seq
        |> Seq.map (Intervals.normalize ~skip_sort:true)
        |> Intervals.Inter.inter_multi_seq ~skip_check:true
        |> CCList.of_seq
      in
      Inter_seq (space, s)
    | Interval_exc (_, start, end_exc) ->
      let space = [ (start, end_exc) ] in
      Interval_exc (space, start, end_exc)
    | Interval_inc (_, start, end_inc) ->
      let space = [ (start, Int64.succ @@ end_inc) ] in
      Interval_inc (space, start, end_inc)
    | Round_robin_pick_list (_, l) ->
      let space, l = aux_list tz l in
      Round_robin_pick_list (space, l)
    | Union_seq (_, s) ->
      let space, s = aux_seq tz s in
      Union_seq (space, s)
    | After (_, b, t1, t2) ->
      let space =
        Intervals.Union.union
          (CCList.to_seq @@ get_search_space t1)
          (CCList.to_seq @@ get_search_space t2)
        |> CCList.of_seq
      in
      After (space, b, t1, t2)
    | Between_inc (_, b, t1, t2) ->
      let space =
        Intervals.Union.union
          (CCList.to_seq @@ get_search_space t1)
          (CCList.to_seq @@ get_search_space t2)
        |> CCList.of_seq
      in
      Between_inc (space, b, t1, t2)
    | Between_exc (_, b, t1, t2) ->
      let space =
        Intervals.Union.union
          (CCList.to_seq @@ get_search_space t1)
          (CCList.to_seq @@ get_search_space t2)
        |> CCList.of_seq
      in
      Between_exc (space, b, t1, t2)
    | Unchunk (_, c) -> Unchunk (aux_chunked tz c, c)
  and aux_chunked tz chunked : search_space =
    match chunked with
    | Unary_op_on_t (_op, time) ->
      let t = aux tz time in
      get_search_space t
    | Unary_op_on_chunked (_op, chunked) -> aux_chunked tz chunked
  and aux_seq tz_offset_s s =
    let s = Seq.map (aux tz_offset_s) s in
    let space =
      Seq.map get_search_space s
      |> Seq.map CCList.to_seq
      |> Intervals.Union.union_multi_seq
      |> CCList.of_seq
    in
    (space, s)
  and aux_list tz_offset_s l =
    l
    |> CCList.to_seq
    |> aux_seq tz_offset_s
    |> fun (space, s) -> (space, CCList.of_seq s)
  in
  aux default_tz time

let propagate_search_space_top_down (time : t) : t =
  let open Time in
  let restrict_search_space time (parent : search_space) (cur : search_space) =
    Intervals.Inter.inter ~skip_check:true (CCList.to_seq parent)
      (CCList.to_seq cur)
    |> CCList.of_seq
    |> calibrate_search_space time
  in
  let rec aux parent_search_space (time : t) : t =
    let stop_propagation = time in
    match time with
    | All -> All
    | Empty -> Empty
    | Timestamp_interval_seq (cur, _) ->
      set_search_space
        (restrict_search_space time parent_search_space cur)
        time
    | Pattern (cur, _) ->
      set_search_space
        (restrict_search_space time parent_search_space cur)
        time
    | Unary_op (cur, op, t) -> (
        match op with
        | Take_points _ | Drop_points _ -> stop_propagation
        | _ ->
          let space = restrict_search_space time parent_search_space cur in
          set_search_space space (Unary_op (cur, op, aux space t)))
    | Interval_exc (cur, _, _) | Interval_inc (cur, _, _) ->
      set_search_space
        (restrict_search_space time parent_search_space cur)
        time
    | Round_robin_pick_list (cur, l) ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space (Round_robin_pick_list (cur, aux_list space l))
    | Inter_seq (cur, s) ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space (Inter_seq (cur, aux_seq space s))
    | Union_seq (cur, s) ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space (Union_seq (cur, aux_seq space s))
    | After (cur, b, t1, t2) ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space (After (cur, b, aux space t1, aux space t2))
    | Between_inc (cur, b, t1, t2) ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space
        (Between_inc (cur, b, aux space t1, aux space t2))
    | Between_exc (cur, b, t1, t2) ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space
        (Between_exc (cur, b, aux space t1, aux space t2))
    | Unchunk (_, _) -> stop_propagation
  and aux_list parent_search_space l = List.map (aux parent_search_space) l
  and aux_seq parent_search_space l = Seq.map (aux parent_search_space) l in
  aux default_search_space time

let optimize_search_space default_tz_offset_s t =
  t
  |> propagate_search_space_bottom_up default_tz_offset_s
  |> propagate_search_space_top_down

type inc_or_exc =
  | Inc
  | Exc

let do_drop_points (n : int64) (s : Time.Interval.t Seq.t) :
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

let do_take_points (n : int64) (s : Time.Interval.t Seq.t) :
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

let do_chunk_at_year_boundary tz (s : Time.Interval.t Seq.t) =
  let open Time in
  let rec aux s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((t1, t2), rest) ->
      let dt1 =
        CCResult.get_exn @@ Date_time'.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time'.of_timestamp ~tz_of_date_time:tz
        |> CCResult.get_exn
      in
      if dt1.year = dt2.year then fun () -> Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time'.set_to_last_month_day_hour_min_sec dt1
          |> Date_time'.to_timestamp
          |> Date_time'.max_of_timestamp_local_result
          |> CCOpt.get_exn
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
        CCResult.get_exn @@ Date_time'.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time'.of_timestamp ~tz_of_date_time:tz
        |> CCResult.get_exn
      in
      if dt1.year = dt2.year && dt1.month = dt2.month then fun () ->
        Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time'.set_to_last_day_hour_min_sec dt1
          |> Date_time'.to_timestamp
          |> Date_time'.max_of_timestamp_local_result
          |> CCOpt.get_exn
          |> Int64.succ
        in
        fun () ->
          Seq.Cons ((t1, t'), aux (fun () -> Seq.Cons ((t', t2), rest)))
  in
  aux s

let dynamic_search_space_adjustment_trigger_size =
  Duration.(make ~days:30 () |> to_seconds)

let inter_minimum_slice_size = Duration.(make ~days:10 () |> to_seconds)

let slice_search_space ~start (t : t) : t =
  get_search_space t
  |> CCList.to_seq
  |> Time.Intervals.Slice.slice ~skip_check:true ~start
  |> CCList.of_seq
  |> calibrate_search_space t
  |> (fun space -> set_search_space space t)
  |> propagate_search_space_top_down

let slice_search_space_multi ~start (l : t list) : t list =
  List.map (slice_search_space ~start) l

let slice_search_space_multi_seq ~start (s : t Seq.t) : t Seq.t =
  Seq.map (slice_search_space ~start) s

let normalize s =
  s
  |> Time.Intervals.normalize ~skip_filter_empty:false ~skip_filter_invalid:true
    ~skip_sort:true
  |> Time.slice_valid_interval

let rec aux search_using_tz time =
  let open Time in
  (match get_search_space time with
   | [] -> Seq.empty
   | _ -> (
       match time with
       | Empty -> Seq.empty
       | All -> CCList.to_seq default_search_space
       | Timestamp_interval_seq (_, s) -> s
       | Pattern (space, pat) -> aux_pattern search_using_tz space pat
       | Unary_op (space, op, t) -> (
           let search_using_tz =
             match op with With_tz x -> x | _ -> search_using_tz
           in
           let s = aux search_using_tz t in
           match op with
           | Not ->
             Intervals.relative_complement ~skip_check:false ~not_mem_of:s
               (CCList.to_seq space)
           | Drop_points n -> do_drop_points (Int64.of_int n) s
           | Take_points n -> do_take_points (Int64.of_int n) s
           | Shift n ->
             Seq.map
               (fun (start, end_exc) ->
                  (Int64.add start n, Int64.add end_exc n))
               s
           | Lengthen n ->
             s
             |> Seq.map (fun (start, end_exc) -> (start, Int64.add end_exc n))
           | With_tz _ -> s)
       | Interval_inc (_, a, b) -> Seq.return (a, Int64.succ b)
       | Interval_exc (_, a, b) -> Seq.return (a, b)
       | Round_robin_pick_list (_, l) ->
         List.map (aux search_using_tz) l
         |> Time.Intervals.Round_robin
            .merge_multi_list_round_robin_non_decreasing ~skip_check:true
       | Inter_seq (_, s) -> aux_inter search_using_tz s
       | Union_seq (_, s) -> aux_union search_using_tz s
       | After (space, b, t1, t2) ->
         let s1 = get_start_spec_of_after search_using_tz space t1 in
         let s2 = aux search_using_tz t2 in
         aux_after search_using_tz space b s1 s2 t1 t2
       | Between_inc (space, b, t1, t2) ->
         let s1 = get_start_spec_of_after search_using_tz space t1 in
         let s2 = aux search_using_tz t2 in
         aux_between Inc search_using_tz space b s1 s2 t1 t2
       | Between_exc (space, b, t1, t2) ->
         let s1 = get_start_spec_of_after search_using_tz space t1 in
         let s2 = aux search_using_tz t2 in
         aux_between Exc search_using_tz space b s1 s2 t1 t2
       | Unchunk (_, c) -> aux_chunked search_using_tz c))
  |> normalize

and aux_pattern search_using_tz space pat =
  let open Time in
  Time_zone.transition_seq search_using_tz
  |> Seq.flat_map (fun ((x, y), entry) ->
      let space =
        Intervals.Inter.inter (Seq.return (x, y)) (CCList.to_seq space)
      in
      let params =
        Seq.map
          (Pattern_resolver.Search_param.make ~search_using_tz
             ~search_using_tz_offset_s:Time_zone.(entry.offset))
          space
      in
      Intervals.Union.union_multi_seq ~skip_check:true
        (Seq.map
           (fun param -> Pattern_resolver.resolve param pat)
           params))

and get_start_spec_of_after search_using_tz space t =
  let search_space_start = fst (List.hd space) in
  aux search_using_tz t
  |> OSeq.drop_while (fun (start, _) -> start < search_space_start)

and get_after_seq_and_maybe_sliced_timere ~start1 ~(s2 : Time.Interval.t Seq.t)
    ~(t2 : t) search_using_tz : Time.Interval.t Seq.t * t =
  match s2 () with
  | Seq.Nil -> (Seq.empty, t2)
  | Seq.Cons ((start2, _), _) ->
    (* we search one extra second back so we can drop contiguous block that spans across start1

       the drop happens at OSeq.drop_while (...)
    *)
    let safe_search_start = start1 -^ 1L in
    let s2, t2 =
      if
        start2 <= start1
        && Int64.sub start1 start2
           >= dynamic_search_space_adjustment_trigger_size
      then
        let timere = slice_search_space ~start:safe_search_start t2 in
        (aux search_using_tz timere, timere)
      else (s2, t2)
    in
    let s2 = OSeq.drop_while (fun (start', _) -> start' < start1) s2 in
    (s2, t2)

and maybe_slice_start_spec_of_after ~last_start2
    ~(rest1 : Time.Interval.t Seq.t) ~(t1 : t) search_using_tz bound :
  Time.Interval.t Seq.t * t =
  match rest1 () with
  | Seq.Nil -> (Seq.empty, t1)
  | Seq.Cons ((start1, _), _) ->
    let distance = last_start2 -^ start1 in
    if
      start1 <= last_start2
      && distance >= bound
      && distance >= dynamic_search_space_adjustment_trigger_size
    then
      let safe_start = last_start2 -^ bound in

      (* we search one extra second back so we can drop contiguous block that spans across safe_start

         the drop happens at OSeq.drop_while (...)
      *)
      let safe_search_start = safe_start -^ 1L in
      let t1 = slice_search_space ~start:safe_search_start t1 in
      let s =
        aux search_using_tz t1
        |> OSeq.drop_while (fun (start', _) -> start' < safe_start)
      in
      (s, t1)
    else (rest1, t1)

and aux_after search_using_tz space bound s1 s2 t1 t2 =
  let _, search_space_end_exc =
    CCOpt.get_exn @@ Misc_utils.last_element_of_list space
  in
  let rec aux_after' s1 s2 t1 t2 =
    match s1 () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start1, _end_exc1), rest1) -> (
        if search_space_end_exc <= start1 then Seq.empty
        else
          let s2, t2 =
            get_after_seq_and_maybe_sliced_timere ~start1 ~s2 ~t2
              search_using_tz
          in
          match s2 () with
          | Seq.Nil -> Seq.empty
          | Seq.Cons ((start2, end_exc2), _) ->
            if search_space_end_exc <= start2 then Seq.empty
            else if Int64.sub start2 start1 <= bound then fun () ->
              Seq.Cons ((start2, end_exc2), aux_after' rest1 s2 t1 t2)
            else
              let s1, t1 =
                maybe_slice_start_spec_of_after ~last_start2:start2 ~rest1 ~t1
                  search_using_tz bound
              in
              aux_after' s1 s2 t1 t2)
  in
  aux_after' s1 s2 t1 t2

and aux_between inc_or_exc search_using_tz space bound s1 s2 t1 t2 =
  let _, search_space_end_exc =
    CCOpt.get_exn @@ Misc_utils.last_element_of_list space
  in
  let rec aux_between' s1 s2 t1 t2 =
    match s1 () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start1, _end_exc1), rest1) -> (
        if search_space_end_exc <= start1 then Seq.empty
        else
          let s2, t2 =
            get_after_seq_and_maybe_sliced_timere ~start1 ~s2 ~t2
              search_using_tz
          in
          match s2 () with
          | Seq.Nil -> Seq.empty
          | Seq.Cons ((start2, end_exc2), _rest2) ->
            if search_space_end_exc <= start2 then Seq.empty
            else if start2 -^ start1 <= bound then
              let interval =
                match inc_or_exc with
                | Inc -> (start1, end_exc2)
                | Exc -> (start1, start2)
              in
              fun () -> Seq.Cons (interval, aux_between' rest1 s2 t1 t2)
            else
              let s1, t1 =
                maybe_slice_start_spec_of_after ~last_start2:start2 ~rest1 ~t1
                  search_using_tz bound
              in
              aux_between' s1 s2 t1 t2)
  in
  aux_between' s1 s2 t1 t2

and aux_union search_using_tz timeres =
  let open Time in
  let resolve_and_merge (s : t Seq.t) : Interval.t Seq.t =
    Seq.map (aux search_using_tz) s
    |> Time.Intervals.Merge.merge_multi_seq ~skip_check:true
  in
  let rec aux_union' (timeres : t Seq.t) (intervals : Interval.t Seq.t) =
    match intervals () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start, end_exc), rest) ->
      let size = end_exc -^ start in
      if size >= dynamic_search_space_adjustment_trigger_size then
        let timeres = slice_search_space_multi_seq ~start:end_exc timeres in
        let next_intervals =
          resolve_and_merge timeres
          |> OSeq.drop_while (fun x -> Time.Interval.le x (start, end_exc))
        in
        fun () ->
          Seq.Cons ((start, end_exc), aux_union' timeres next_intervals)
      else fun () -> Seq.Cons ((start, end_exc), aux_union' timeres rest)
  in
  aux_union' timeres (resolve_and_merge timeres)

and aux_inter search_using_tz timeres =
  let open Time in
  let resolve ~start search_using_tz timeres =
    List.map
      (fun timere ->
         aux search_using_tz timere
         |> Intervals.Slice.slice ~skip_check:true ~start)
      timeres
  in
  let collect_batch (l : Interval.t Seq.t list) : Interval.t option list =
    List.map
      (fun s -> match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x)
      l
  in
  let rec aux_inter' ~start (timeres : t list) =
    let interval_batches = resolve ~start search_using_tz timeres in
    let batch_for_sampling = collect_batch interval_batches in
    if List.exists CCOpt.is_none batch_for_sampling then Seq.empty
    else
      let batch_for_sampling =
        CCList.filter_map CCFun.id batch_for_sampling
        |> Intervals.Sort.sort_uniq_intervals_list ~skip_check:true
      in
      match batch_for_sampling with
      | [] -> Seq.empty
      | _ ->
        let _min_start, min_end_exc = List.hd batch_for_sampling in
        let max_start, max_end_exc =
          CCOpt.get_exn @@ Misc_utils.last_element_of_list batch_for_sampling
        in
        let timeres, interval_batches =
          if
            min_end_exc <= max_start
            && max_start -^ min_end_exc
               >= dynamic_search_space_adjustment_trigger_size
          then
            let timeres = slice_search_space_multi ~start:max_start timeres in
            (timeres, resolve ~start search_using_tz timeres)
          else (timeres, interval_batches)
        in
        let end_exc =
          if max_end_exc -^ start <= inter_minimum_slice_size then
            start +^ inter_minimum_slice_size
          else max_end_exc
        in
        let intervals_up_to_end_exc =
          interval_batches
          |> CCList.to_seq
          |> Seq.map (Intervals.Slice.slice ~skip_check:true ~end_exc)
          |> Intervals.Inter.inter_multi_seq ~skip_check:true
        in
        fun () ->
          Seq.Cons (intervals_up_to_end_exc, aux_inter' ~start:end_exc timeres)
  in
  aux_inter' ~start:default_search_space_start (CCList.of_seq timeres)
  |> Seq.flat_map CCFun.id

and aux_chunked search_using_tz (chunked : chunked) =
  let open Time in
  let chunk_based_on_op_on_t op s =
    match op with
    | Chunk_disjoint_interval -> normalize s
    | Chunk_by_duration { chunk_size; drop_partial } ->
      Intervals.chunk ~skip_check:true ~drop_partial ~chunk_size s
    | Chunk_at_year_boundary -> do_chunk_at_year_boundary search_using_tz s
    | Chunk_at_month_boundary -> do_chunk_at_month_boundary search_using_tz s
  in
  match chunked with
  | Unary_op_on_t (op, t) -> aux search_using_tz t |> chunk_based_on_op_on_t op
  | Unary_op_on_chunked (op, c) -> (
      let s = aux_chunked search_using_tz c in
      match op with
      | Nth n -> s |> OSeq.drop n |> OSeq.take 1
      | Drop n -> OSeq.drop n s
      | Take n -> OSeq.take n s
      | Take_nth n -> OSeq.take_nth n s
      | Chunk_again op -> chunk_based_on_op_on_t op s)

let resolve' ~(search_using_tz) (time : t) :
  (Time.Interval.t Seq.t, string) result =
  let open Time in
  try
    Ok (time |> optimize_search_space search_using_tz |> aux search_using_tz)
  with
  | Interval_is_invalid -> Error "Invalid interval"
  | Intervals_are_not_sorted -> Error "Intervals are not sorted"

let resolve ?(search_using_tz = Time_zone.utc) (time : Time_ast.t) :
  (Time.Interval.t Seq.t, string) result =
  resolve' ~search_using_tz (t_of_ast time)
