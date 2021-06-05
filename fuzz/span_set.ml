include Diet.Make (struct
  type t = Timedesc.Span.t

  let compare = Timedesc.Span.compare

  let zero = Timedesc.Span.zero

  let pred = Timedesc.Span.pred

  let succ = Timedesc.Span.succ

  let sub = Timedesc.Span.sub

  let add = Timedesc.Span.add

  let to_string = Timedesc.Span.to_string
end)
