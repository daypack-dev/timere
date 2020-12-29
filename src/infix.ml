let ( & ) a b = Time.inter [ a; b ]

let ( ||| ) a b = Time.union [ a; b ]

let ( -- ) = Time.interval_inc

let ( --^ ) = Time.interval_exc

let ( >> ) (f1 : Time.chunked -> Time.chunked) (f2 : Time.chunked -> Time.chunked) :
  Time.chunked -> Time.chunked =
  fun x -> x |> f1 |> f2
