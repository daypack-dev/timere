include CCMap.Make (struct
    type t = string

    let compare = String.compare
  end)
