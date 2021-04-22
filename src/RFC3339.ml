open Date_time_components

let frac_s_1_divisor = Span.ns_count_in_s / 10

let frac_s_2_divisor = frac_s_1_divisor / 10

let frac_s_3_divisor = frac_s_2_divisor / 10

let frac_s_4_divisor = frac_s_3_divisor / 10

let frac_s_5_divisor = frac_s_4_divisor / 10

let frac_s_6_divisor = frac_s_5_divisor / 10

let frac_s_7_divisor = frac_s_6_divisor / 10

let frac_s_8_divisor = frac_s_7_divisor / 10

let frac_s_9_divisor = frac_s_8_divisor / 10

let get_divisor frac_s =
  match frac_s with
  | 1 -> frac_s_1_divisor
  | 2 -> frac_s_2_divisor
  | 3 -> frac_s_3_divisor
  | 4 -> frac_s_4_divisor
  | 5 -> frac_s_5_divisor
  | 6 -> frac_s_6_divisor
  | 7 -> frac_s_7_divisor
  | 8 -> frac_s_8_divisor
  | 9 -> frac_s_9_divisor
  | _ -> failwith "Unexpected case"

let deduce_smallest_lossless_frac_s ~ns =
  if ns = 0 then 0
  else if ns mod frac_s_1_divisor = 0 then 1
  else if ns mod frac_s_2_divisor = 0 then 2
  else if ns mod frac_s_3_divisor = 0 then 3
  else if ns mod frac_s_4_divisor = 0 then 4
  else if ns mod frac_s_5_divisor = 0 then 5
  else if ns mod frac_s_6_divisor = 0 then 6
  else if ns mod frac_s_7_divisor = 0 then 7
  else if ns mod frac_s_8_divisor = 0 then 8
  else 9

let pp_date_time ?frac_s () formatter (dt : Time.Date_time'.t) =
  let ns = dt.ns mod Span.ns_count_in_s in
  let frac_s =
    match frac_s with
    | None -> deduce_smallest_lossless_frac_s ~ns
    | Some x -> x
  in
  if frac_s < 0 then invalid_arg "pp_date_time: frac_s cannot be < 0"
  else if frac_s > 9 then invalid_arg "pp_date_time: frac_s cannot be > 9"
  else
    match tz_offset_of_tz_info dt.tz_info with
    | None -> raise (Printers.Date_time_cannot_deduce_tz_offset_s dt)
    | Some offset ->
      let tz_off =
        if Duration.(equal offset zero) then "Z"
        else
          let sign = match offset.sign with `Pos -> '+' | `Neg -> '-' in
          Printf.sprintf "%c%02d:%02d" sign offset.hours offset.minutes
      in
      let second =
        if Time.Date_time'.is_leap_second dt then 60 else dt.second
      in
      if frac_s = 0 then
        Fmt.pf formatter "%04d-%02d-%02dT%02d:%02d:%02d%s" dt.year
          (human_int_of_month dt.month)
          dt.day dt.hour dt.minute second tz_off
      else
        let divisor = get_divisor frac_s in
        Fmt.pf formatter "%04d-%02d-%02dT%02d:%02d:%02d.%0*d%s" dt.year
          (human_int_of_month dt.month)
          dt.day dt.hour dt.minute second frac_s (ns / divisor) tz_off

let of_date_time ?frac_s (dt : Time.Date_time'.t) : string option =
  try Some (Fmt.str "%a" (pp_date_time ?frac_s ()) dt)
  with Printers.Date_time_cannot_deduce_tz_offset_s _ -> None

let pp_timestamp ?frac_s () formatter (x : Span.t) =
  match Time.Date_time'.of_timestamp ~tz_of_date_time:Time_zone.utc x with
  | None -> invalid_arg "Invalid timestamp"
  | Some dt -> Fmt.pf formatter "%a" (pp_date_time ?frac_s ()) dt

let of_timestamp ?frac_s (x : Span.t) : string =
  Fmt.str "%a" (pp_timestamp ?frac_s ()) x
