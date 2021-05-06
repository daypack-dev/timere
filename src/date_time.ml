type t = {
  year : int;
  month : int;
  day : int;
  hour : int;
  minute : int;
  second : int;
  ns : int;
}

let set_to_first_ns (x : t) : t = { x with ns = 0 }

  let set_to_last_ns (x : t) : t = { x with ns = Span.ns_count_in_s - 1 }

  let set_to_first_sec_ns (x : t) : t = { x with second = 0 } |> set_to_first_ns

  let set_to_last_sec_ns (x : t) : t = { x with second = 59 } |> set_to_last_ns

  let set_to_first_min_sec_ns (x : t) : t =
    { x with minute = 0 } |> set_to_first_sec_ns

  let set_to_last_min_sec_ns (x : t) : t =
    { x with minute = 59 } |> set_to_last_sec_ns

  let set_to_first_hour_min_sec_ns (x : t) : t =
    { x with hour = 0 } |> set_to_first_min_sec_ns

  let set_to_last_hour_min_sec_ns (x : t) : t =
    { x with hour = 23 } |> set_to_last_min_sec_ns

  let set_to_first_day_hour_min_sec_ns (x : t) : t =
    { x with day = 1 } |> set_to_first_hour_min_sec_ns

  let set_to_last_day_hour_min_sec_ns (x : t) : t =
    { x with day = day_count_of_month ~year:x.year ~month:x.month }
    |> set_to_last_hour_min_sec_ns

  let set_to_first_month_day_hour_min_sec_ns (x : t) : t =
    { x with month = 1 } |> set_to_first_day_hour_min_sec_ns

  let set_to_last_month_day_hour_min_sec_ns (x : t) : t =
    { x with month = 12 } |> set_to_last_day_hour_min_sec_ns
