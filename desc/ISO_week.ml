open Date_time_utils

let fixed_to_weekday = `Mon

type t = { jd : int }

type error =
  [ `Does_not_exist
  | `Invalid_iso_week_year of int
  | `Invalid_iso_week of int
  ]

exception Error_exn of error

let make ~year ~week : (t, error) result =
  if year < Constants.min_year || Constants.max_year < year
  then Error (`Invalid_iso_week_year year)
  else if week < 1 || week_count_of_iso_week_year ~year < week
  then Error (`Invalid_iso_week week)
  else
    Ok
      {
        jd =
          jd_of_iso_week_date ~year ~week ~weekday:fixed_to_weekday;
      }

let make_exn ~year ~week : t =
  match make ~year ~week with
  | Error e -> raise (Error_exn e)
  | Ok x -> x

let iso_year_and_week t : int * int =
  let year, week, _ = iso_week_date_of_jd t.jd in
  (year, week)

let year t : int = fst @@ iso_year_and_week t

let week t : int = snd @@ iso_year_and_week t

let sub ~week (t : t) : t = { jd = t.jd - (week * 7) }

let add ~week (t : t) : t = { jd = t.jd + (week * 7) }

let diff_week t1 t2 = (t1.jd - t2.jd) / 7
