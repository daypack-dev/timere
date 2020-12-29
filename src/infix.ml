let ( & ) a b = Time.inter [ a; b ]

let ( ||| ) a b = Time.union [ a; b ]

let ( -- ) = Time.interval_inc

let ( --^ ) = Time.interval_exc

let ( >> ) (f1 : 'a -> 'a) (f2 : 'a -> 'a) : 'a -> 'a = fun x -> x |> f1 |> f2
