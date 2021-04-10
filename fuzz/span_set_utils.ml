let intervals_of_int64s (s : int64 Seq.t) :
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
  |> Seq.map (fun (x, y) ->
      Span.make ~s:x (), Span.make ~s:y ()
    )

let span_set_of_intervals (s : Time.Interval.t Seq.t) : Span_set.t =
  Seq.fold_left (fun acc (x, y) ->
      Span_set.(add (Interval.make x (Span.pred y)) acc)
    )
    Span_set.empty
    s

