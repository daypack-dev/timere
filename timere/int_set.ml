include CCSet.Make (struct
    type t = int

    let compare = compare
  end)
