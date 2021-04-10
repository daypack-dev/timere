include Diet.Make (struct
    type t = Span.t

    let compare = Span.compare

    let zero = Span.zero

    let pred = Span.pred

    let succ = Span.succ

    let sub = Span.sub

    let add = Span.add

    let to_string = Printers.string_of_span
  end)
