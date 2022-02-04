open Date_time_utils

let fixed_to_weekday = `Mon

type t = { jd : int }

let equal (x : t) (y : t) = x.jd = y.jd

let lt (x : t) (y : t) = x.jd < y.jd

let le (x : t) (y : t) = x.jd <= y.jd

let gt (x : t) (y : t) = x.jd > y.jd

let ge (x : t) (y : t) = x.jd >= y.jd

let compare (x : t) (y : t) = compare x.jd y.jd

type error =
  [ `Does_not_exist
  | `Invalid_iso_year of int
  | `Invalid_iso_week of int
  ]

exception Error_exn of error

let make ~year ~week : (t, error) result =
  if year < Constants.min_year || Constants.max_year < year then
    Error (`Invalid_iso_year year)
  else if week < 1 || week_count_of_iso_year ~year < week then
    Error (`Invalid_iso_week week)
  else Ok { jd = jd_of_iso_week_date ~year ~week ~weekday:fixed_to_weekday }

let make_exn ~year ~week : t =
  match make ~year ~week with Error e -> raise (Error_exn e) | Ok x -> x

let year_week t : int * int =
  let year, week, weekday = iso_week_date_of_jd t.jd in
  assert (weekday = fixed_to_weekday);
  (year, week)

let year t : int = fst @@ year_week t

let week t : int = snd @@ year_week t

let sub ~weeks (t : t) : t = { jd = t.jd - (weeks * 7) }

let add ~weeks (t : t) : t = { jd = t.jd + (weeks * 7) }

let diff_weeks t1 t2 = (t1.jd - t2.jd) / 7
