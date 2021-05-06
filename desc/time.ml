type t = {
  hour : int;
  minute : int;
  second : int;
  ns : int;
}

type error =
  [ `Invalid_hour of int
  | `Invalid_minute of int
  | `Invalid_second of int
  | `Invalid_frac of float
  | `Invalid_ns of int
  ]

let make ~hour ~minute ~second ~ns ~frac : (t, error) result =
  if hour < 0 || 23 < hour then Error (`Invalid_hour hour)
  else if minute < 0 || 59 < minute then Error (`Invalid_minute minute)
  else if second < 0 || 60 < second then Error (`Invalid_second second)
  else if frac < 0. then Error (`Invalid_frac frac)
  else if ns < 0 then Error (`Invalid_ns ns)
  else
    let ns = ns + int_of_float (frac *. Span.ns_count_in_s_float) in
    if ns >= Span.ns_count_in_s then Error (`Invalid_ns ns) else
      let is_leap_second = second = 60 in
      let second = if is_leap_second then 59 else second in
      let ns =
        if is_leap_second then
          ns + Span.ns_count_in_s
        else
          ns
      in
      Ok { hour; minute; second; ns }

let to_span (x : t) : Span.t =
  Span.For_human'.make_exn ~hours:x.hour ~minutes:x.minute ~seconds:x.second
    ~ns:(x.ns mod Span.ns_count_in_s) ()
