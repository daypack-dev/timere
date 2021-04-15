let span_set_max_interval_count = 1_000_000

let span_set_full : Span_set.t =
  Span_set.add
    (Span_set.Interval.make Time.timestamp_min (Span.pred Time.timestamp_max))
    Span_set.empty

let span_set_map (f : Time.Interval'.t -> Time.Interval'.t) (set : Span_set.t) :
  Span_set.t =
  Span_set.fold
    (fun interval acc ->
       let x, y = Span_set.Interval.(x interval, y interval) in
       let y = Span.succ y in
       let x, y = f (x, y) in
       let y = Span.pred y in
       Span_set.add (Span_set.Interval.make x y) acc)
    set Span_set.empty

let intervals_of_int64s (s : int64 Seq.t) : Time.Interval'.t Seq.t =
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
  aux None s |> Seq.map (fun (x, y) -> (Span.make ~s:x (), Span.make ~s:y ()))

let span_set_of_intervals (s : Time.Interval'.t Seq.t) : Span_set.t =
  Seq.fold_left
    (fun (count, acc) (x, y) ->
       if count >= span_set_max_interval_count then Crowbar.bad_test ()
       else if Span.(x = y) then (count, acc)
       else (succ count, Span_set.(add (Interval.make x (Span.pred y)) acc)))
    (0, Span_set.empty) s
  |> snd

let intervals_of_span_set (set : Span_set.t) : Time.Interval'.t Seq.t =
  let rec aux set =
    match Span_set.min_elt set with
    | i ->
      fun () ->
        Seq.Cons
          ( Span_set.Interval.(x i, Span.succ (y i)),
            aux (Span_set.remove i set) )
    | exception Not_found -> Seq.empty
  in
  aux set
