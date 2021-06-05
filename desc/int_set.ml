include CCSet.Make (struct
  type t = int

  let compare = compare
end)

let to_seq x = x |> to_list |> CCList.to_seq
