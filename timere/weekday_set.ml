include CCSet.Make (struct
    type t = Timedesc.weekday

    let compare = compare
  end)

let to_seq x = x |> to_list |> CCList.to_seq
