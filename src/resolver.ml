open Int64_utils
open Time_ast

type search_space = Time.Interval.t list

let default_search_space_start = Time.timestamp_min

let default_search_space_end_exc = Time.timestamp_max

let default_search_space : search_space =
  [ (default_search_space_start, default_search_space_end_exc) ]

type t =
  | Empty
  | All
  | Intervals of search_space * (int64 * int64) Seq.t
  | Pattern of search_space * Pattern.t
  | Unary_op of search_space * unary_op * t
  | Inter_seq of search_space * t Seq.t
  | Union_seq of search_space * t Seq.t
  | Bounded_intervals of {
      search_space : search_space;
      pick : [ `Whole | `Snd ];
      bound : int64;
      start : Points.t;
      end_exc : Points.t;
    }
  | Unchunk of search_space * chunked

and chunked =
  | Unary_op_on_t of chunked_unary_op_on_t * t
  | Unary_op_on_chunked of chunked_unary_op_on_chunked * chunked

let rec t_of_ast (ast : Time_ast.t) : t =
  match ast with
  | Empty -> Empty
  | All -> All
  | Intervals s -> Intervals (default_search_space, s)
  | Pattern p -> Pattern (default_search_space, p)
  | Unary_op (op, t) -> Unary_op (default_search_space, op, t_of_ast t)
  | Inter_seq s -> Inter_seq (default_search_space, Seq.map t_of_ast s)
  | Union_seq s -> Union_seq (default_search_space, Seq.map t_of_ast s)
  | Bounded_intervals { pick; bound; start; end_exc } ->
    Bounded_intervals
      { search_space = default_search_space; pick; bound; start; end_exc }
  | Unchunk chunked ->
    Unchunk (default_search_space, chunked_of_ast_chunked chunked)

and chunked_of_ast_chunked (c : Time_ast.chunked) : chunked =
  match c with
  | Unary_op_on_t (op, t) -> Unary_op_on_t (op, t_of_ast t)
  | Unary_op_on_chunked (op, chunked) ->
    Unary_op_on_chunked (op, chunked_of_ast_chunked chunked)

let get_search_space (time : t) : Time.Interval.t list =
  match time with
  | All -> default_search_space
  | Empty -> []
  | Intervals (s, _) -> s
  | Pattern (s, _) -> s
  | Unary_op (s, _, _) -> s
  | Inter_seq (s, _) -> s
  | Union_seq (s, _) -> s
  | Bounded_intervals { search_space; _ } -> search_space
  | Unchunk (s, _) -> s

let calibrate_search_space_for_set (time : t) space : search_space =
  match time with
  | All | Empty | Intervals _ | Pattern _ -> space
  | Unary_op (_, op, _) -> (
      match op with
      | Shift n -> List.map (fun (x, y) -> (Int64.sub x n, Int64.sub y n)) space
      | _ -> space)
  | Inter_seq _ | Union_seq _ -> space
  | Bounded_intervals { bound; _ } -> (
      match space with
      | [] -> []
      | (x, y) :: rest -> (Int64.sub x bound, y) :: rest)
  | Unchunk _ -> space

let set_search_space space (time : t) : t =
  match time with
  | All -> All
  | Empty -> Empty
  | Intervals (_, x) -> Intervals (space, x)
  | Pattern (_, x) -> Pattern (space, x)
  | Unary_op (_, op, x) -> Unary_op (space, op, x)
  | Inter_seq (_, x) -> Inter_seq (space, x)
  | Union_seq (_, x) -> Union_seq (space, x)
  | Bounded_intervals { search_space = _; pick; bound; start; end_exc } ->
    Bounded_intervals { search_space = space; pick; bound; start; end_exc }
  | Unchunk (_, c) -> Unchunk (space, c)

let search_space_of_year_range tz year_range =
  let open Time in
  let aux_start start =
    Date_time'.set_to_first_month_day_hour_min_sec
      { Date_time'.min with year = start; tz_info = `Tz_only tz }
    |> Date_time'.to_timestamp
    |> Date_time'.min_of_timestamp_local_result
  in
  let aux_end_inc end_exc =
    Date_time'.set_to_last_month_day_hour_min_sec
      { Date_time'.min with year = end_exc; tz_info = `Tz_only tz }
    |> Date_time'.to_timestamp
    |> Date_time'.min_of_timestamp_local_result
    |> Int64.succ
  in
  let aux_end_exc end_exc =
    Date_time'.set_to_first_month_day_hour_min_sec
      { Date_time'.min with year = end_exc; tz_info = `Tz_only tz }
    |> Date_time'.to_timestamp
    |> Date_time'.min_of_timestamp_local_result
  in
  match year_range with
  | `Range_inc (start, end_inc) -> (aux_start start, aux_end_inc end_inc)
  | `Range_exc (start, end_exc) -> (aux_start start, aux_end_exc end_exc)

let search_space_of_year tz year =
  search_space_of_year_range tz (`Range_inc (year, year))

let empty_search_space = []

let propagate_search_space_bottom_up default_tz (time : t) : t =
  let open Time in
  let rec aux (tz : Time_zone.t) (time : t) : t =
    match time with
    | All -> All
    | Empty -> Empty
    | Intervals (_, s) -> (
        match s () with
        | Seq.Nil -> time
        | Seq.Cons ((start, _), _) ->
          Intervals ([ (start, default_search_space_end_exc) ], s))
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
        | Shift n ->
          let space =
            get_search_space t
            |> List.map (fun (x, y) -> (Int64.add x n, Int64.add y n))
          in
          Unary_op (space, op, t)
        | Lengthen n ->
          let space =
            get_search_space t |> List.map (fun (x, y) -> (x, Int64.add y n))
          in
          Unary_op (space, op, t)
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
    | Union_seq (_, s) ->
      let space, s = aux_seq tz s in
      Union_seq (space, s)
    | Bounded_intervals { search_space = _; pick; bound; start; end_exc } ->
      let search_space =
        match Time.Date_time'.of_points start with
        | None -> default_search_space
        | Some dt ->
          let space_start =
            dt
            |> Time.Date_time'.to_timestamp
            |> Time.Date_time'.min_of_timestamp_local_result
          in
          let space_end_exc =
            dt
            |> Time.Date_time'.to_timestamp
            |> Time.Date_time'.max_of_timestamp_local_result
            |> Int64.add bound
          in
          [ (space_start, space_end_exc) ]
      in
      Bounded_intervals { search_space; pick; bound; start; end_exc }
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
  in
  aux default_tz time

let propagate_search_space_top_down (time : t) : t =
  let open Time in
  let restrict_search_space time (parent : search_space) (cur : search_space) =
    Intervals.Inter.inter ~skip_check:true (CCList.to_seq parent)
      (CCList.to_seq cur)
    |> CCList.of_seq
    |> calibrate_search_space_for_set time
  in
  let rec aux parent_search_space (time : t) : t =
    let stop_propagation = time in
    match time with
    | All -> All
    | Empty -> Empty
    | Intervals (cur, _) ->
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
    | Inter_seq (cur, s) ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space (Inter_seq (cur, aux_seq space s))
    | Union_seq (cur, s) ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space (Union_seq (cur, aux_seq space s))
    | Bounded_intervals { search_space = cur; pick; bound; start; end_exc } ->
      let space = restrict_search_space time parent_search_space cur in
      set_search_space space
        (Bounded_intervals { search_space = cur; pick; bound; start; end_exc })
    | Unchunk (_, _) -> stop_propagation
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
        CCOpt.get_exn @@ Date_time'.of_timestamp ~tz_of_date_time:tz t1
      in
      let dt2 =
        t2
        |> Int64.pred
        |> Date_time'.of_timestamp ~tz_of_date_time:tz
        |> CCOpt.get_exn
      in
      if dt1.year = dt2.year then fun () -> Seq.Cons ((t1, t2), aux rest)
      else
        let t' =
          Date_time'.set_to_last_month_day_hour_min_sec dt1
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

let dynamic_search_space_adjustment_trigger_size =
  Duration.(make ~days:30 () |> to_seconds)

let inter_minimum_slice_size = Duration.(make ~days:10 () |> to_seconds)

let slice_search_space ~start (t : t) : t =
  get_search_space t
  |> CCList.to_seq
  |> Time.Intervals.Slice.slice ~skip_check:true ~start
  |> CCList.of_seq
  |> calibrate_search_space_for_set t
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

let aux_pattern search_using_tz space pat =
  let open Time in
  let space = CCList.to_seq space in
  Time_zone.Raw.to_transition_seq search_using_tz
  |> Seq.flat_map (fun ((x, y), entry) ->
      let space = Intervals.Inter.inter (Seq.return (x, y)) space in
      let params =
        Seq.map
          (Pattern_resolver.Search_param.make ~search_using_tz
             ~search_using_tz_offset_s:Time_zone.(entry.offset))
          space
      in
      Intervals.Union.union_multi_seq ~skip_check:true
        (Seq.map (fun param -> Pattern_resolver.resolve param pat) params))

let aux_points search_using_tz space (p, tz_info) : timestamp Seq.t =
  let search_using_tz =
    match tz_info with
    | None -> search_using_tz
    | Some tz_info -> (
        match tz_info with
        | `Tz_only tz -> tz
        | `Tz_offset_s_only x -> Time_zone.make_offset_only x
        | `Tz_and_tz_offset_s (tz, _) -> tz)
  in
  aux_pattern search_using_tz space (Points.to_pattern (p, tz_info))
  |> Seq.map (fun (x, y) ->
      assert (y = Int64.succ x);
      x)

let rec aux search_using_tz time =
  let open Time in
  (match get_search_space time with
   | [] -> Seq.empty
   | _ -> (
       match time with
       | Empty -> Seq.empty
       | All -> CCList.to_seq default_search_space
       | Intervals (_, s) -> s
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
       | Inter_seq (_, s) -> aux_inter search_using_tz s
       | Union_seq (_, s) -> aux_union search_using_tz s
       | Bounded_intervals { search_space; pick; bound; start; end_exc } ->
         aux_bounded_intervals search_using_tz search_space pick bound start
           end_exc
       | Unchunk (_, c) -> aux_chunked search_using_tz c))
  |> normalize

(* and aux_follow search_using_tz space bound s1 s2 t1 t2 =
 *   let _, search_space_end_exc =
 *     CCOpt.get_exn @@ Misc_utils.last_element_of_list space
 *   in
 *   let rec aux_follow' s1 s2 t1 t2 =
 *     match s1 () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons ((start1, _end_exc1), rest1) -> (
 *         if search_space_end_exc <= start1 then Seq.empty
 *         else
 *           let s2, t2 =
 *             get_points_after_start1
 *             ~start1 ~p2 ~p2
 *               search_using_tz
 *           in
 *           match s2 () with
 *           | Seq.Nil -> Seq.empty
 *           | Seq.Cons ((start2, end_exc2), _) ->
 *             if search_space_end_exc <= start2 then Seq.empty
 *             else if Int64.sub start2 start1 <= bound then fun () ->
 *               Seq.Cons ((start2, end_exc2), aux_follow' rest1 s2 t1 t2)
 *             else
 *               let s1, t1 =
 *                 maybe_slice_start_spec_of_follow ~last_start2:start2 ~rest1
 *                   ~t1 search_using_tz bound
 *               in
 *               aux_follow' s1 s2 t1 t2)
 *   in
 *   aux_follow' s1 s2 t1 t2 *)
and get_points_after_start1 ~start1 ~(s2 : timestamp Seq.t) ~(p2 : Points.t)
    search_using_tz space : timestamp Seq.t * search_space =
  match s2 () with
  | Seq.Nil -> (Seq.empty, space)
  | Seq.Cons (start2, _) ->
    if
      start2 < start1
      && start1 -^ start2 >= dynamic_search_space_adjustment_trigger_size
    then
      let space =
        space
        |> CCList.to_seq
        |> Time.Intervals.Slice.slice ~start:(Int64.succ start1)
        |> CCList.of_seq
      in
      (aux_points search_using_tz space p2, space)
    else (OSeq.drop_while (fun start2 -> start2 <= start1) s2, space)

and skip_points_in_p1 ~last_start2 ~(rest1 : timestamp Seq.t) ~(p1 : Points.t)
    search_using_tz bound space : timestamp Seq.t * search_space =
  match rest1 () with
  | Seq.Nil -> (Seq.empty, space)
  | Seq.Cons (start1, _) ->
    let distance = last_start2 -^ start1 in
    if
      start1 <= last_start2
      && distance >= bound
      && distance >= dynamic_search_space_adjustment_trigger_size
    then
      let search_start = last_start2 -^ bound in
      let space =
        space
        |> CCList.to_seq
        |> Time.Intervals.Slice.slice ~start:search_start
        |> CCList.of_seq
      in
      (aux_points search_using_tz space p1, space)
    else (rest1, space)

and aux_bounded_intervals search_using_tz space pick bound p1 p2 =
  let _, search_space_end_exc =
    CCOpt.get_exn @@ Misc_utils.last_element_of_list space
  in
  let rec aux_bounded_intervals' s1 s2 space1 space2 p1 p2 =
    match s1 () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (start1, rest1) -> (
        if search_space_end_exc <= start1 then Seq.empty
        else
          let s2, space2 =
            get_points_after_start1 ~start1 ~s2 ~p2 search_using_tz space2
          in
          match s2 () with
          | Seq.Nil -> Seq.empty
          | Seq.Cons (start2, _rest2) ->
            if search_space_end_exc <= start2 then Seq.empty
            else if start2 -^ start1 <= bound then
              let interval =
                match pick with
                | `Whole -> (start1, start2)
                | `Snd -> (start2, Int64.succ start2)
              in
              fun () ->
                Seq.Cons
                  ( interval,
                    aux_bounded_intervals' rest1 s2 space1 space2 p1 p2 )
            else
              let s1, space1 =
                skip_points_in_p1 ~last_start2:start2 ~rest1 ~p1
                  search_using_tz bound space1
              in
              aux_bounded_intervals' s1 s2 space1 space2 p1 p2)
  in
  aux_bounded_intervals'
    (aux_points search_using_tz space p1)
    (aux_points search_using_tz space p2)
    space space p1 p2

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

let resolve' ~search_using_tz (time : t) :
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
