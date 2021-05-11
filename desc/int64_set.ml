include CCSet.Make (struct
    type t = int64

    let compare = compare
  end)
