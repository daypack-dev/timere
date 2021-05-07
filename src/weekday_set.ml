include CCSet.Make (struct
    type t = Timedesc.weekday

    let compare = compare
  end)
