open Date_time_utils

let fixed_to_weekday = `Mon

type t = { jd : int }

type error =
  [ `Does_not_exist
  | `Invalid_iso_week_year of int
  | `Invalid_iso_week of int
  ]

exception Error_exn of error

let make ~iso_week_year ~iso_week : (t, error) result =
  if iso_week_year < Constants.min_year || Constants.max_year < iso_week_year
  then Error (`Invalid_iso_week_year iso_week_year)
  else if iso_week < 1 || week_count_of_iso_week_year ~iso_week_year < iso_week
  then Error (`Invalid_iso_week iso_week)
  else
    Ok
      {
        jd =
          jd_of_iso_week_date ~iso_week_year ~iso_week ~weekday:fixed_to_weekday;
      }

let make_exn ~iso_week_year ~iso_week : t =
  match make ~iso_week_year ~iso_week with
  | Error e -> raise (Error_exn e)
  | Ok x -> x

let iso_week_year_and_week t : int * int =
  let iso_week_year, iso_week, _ = iso_week_date_of_jd t.jd in
  (iso_week_year, iso_week)

let iso_week_year t : int = fst @@ iso_week_year_and_week t

let iso_week t : int = snd @@ iso_week_year_and_week t

let sub_week (t : t) n : t = { jd = t.jd - (n * 7) }

let add_week (t : t) n : t = { jd = t.jd + (n * 7) }

let diff_week t1 t2 = (t1.jd - t2.jd) / 7
