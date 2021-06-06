let ( &&& ) a b = Time.inter [ a; b ]

let ( ||| ) a b = Time.union [ a; b ]

let ( %> ) (f1 : 'a -> 'b) (f2 : 'b -> 'c) : 'a -> 'c = fun x -> x |> f1 |> f2
