type t = { month_count : int }

let equal (x : t) (y : t) = x.month_count = y.month_count

let lt (x : t) (y : t) = x.month_count < y.month_count

let le (x : t) (y : t) = x.month_count <= y.month_count

let gt (x : t) (y : t) = x.month_count > y.month_count

let ge (x : t) (y : t) = x.month_count >= y.month_count

let compare (x : t) (y : t) = compare x.month_count y.month_count

let month_count_of_ym ~year ~month = (year * 12) + (month - 1)

let ym_of_month_count month_count =
  if month_count >= 0 then (month_count / 12, (month_count mod 12) + 1)
  else ((month_count - 11) / 12, (((month_count mod 12) + 12) mod 12) + 1)

type error =
  [ `Does_not_exist
  | `Invalid_year of int
  | `Invalid_month of int
  ]

exception Error_exn of error

let make ~year ~month : (t, error) result =
  if year < Constants.min_year || Constants.max_year < year then
    Error (`Invalid_year year)
  else if month < 1 || 12 < month then Error (`Invalid_month month)
  else Ok { month_count = month_count_of_ym ~year ~month }

let make_exn ~year ~month : t =
  match make ~year ~month with Error e -> raise (Error_exn e) | Ok x -> x

let sub_month_count (t : t) n : t = { month_count = t.month_count - n }

let sub ?(years = 0) ?(months = 0) t : t =
  sub_month_count t ((years * 12) + months)

let add_month_count (t : t) n : t = { month_count = t.month_count + n }

let add ?(years = 0) ?(months = 0) t : t =
  add_month_count t ((years * 12) + months)

let diff_months t1 t2 = t1.month_count - t2.month_count

let year_month (t : t) : int * int = ym_of_month_count t.month_count

let year (t : t) : int = fst @@ year_month t

let month (t : t) : int = snd @@ year_month t
