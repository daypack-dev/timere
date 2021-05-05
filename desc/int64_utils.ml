let ( +^ ) = Int64.add

let ( -^ ) = Int64.sub

let ( /^ ) = Int64.div

let ( *^ ) = Int64.mul

let div_round_up a b = (a +^ b -^ 1L) /^ b
