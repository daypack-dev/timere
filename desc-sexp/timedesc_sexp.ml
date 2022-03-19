module Date = struct
  let to_sexp = To_sexp.sexp_of_date

  let to_sexp_string x = CCSexp.to_string (To_sexp.sexp_of_date x)

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.date_of_sexp

  let of_sexp_string =
    Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.date_of_sexp

  let pp_sexp = Printers.wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_date
end

module Time = struct
  let to_sexp = To_sexp.sexp_of_time

  let to_sexp_string x = CCSexp.to_string (To_sexp.sexp_of_time x)

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.time_of_sexp

  let of_sexp_string =
    Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.time_of_sexp

  let pp_sexp = Printers.wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_time
end

module Span = struct
  let to_sexp = To_sexp.sexp_of_span

  let to_sexp_string x = CCSexp.to_string (To_sexp.sexp_of_span x)

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.span_of_sexp

  let of_sexp_string =
    Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.span_of_sexp

  let pp_sexp = Printers.wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_span
end

module Timestamp = struct
  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.span_of_sexp

  let to_sexp = To_sexp.sexp_of_span
end

let to_sexp = To_sexp.sexp_of_date_time

let to_sexp_string x = CCSexp.to_string (To_sexp.sexp_of_date_time x)

let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.date_time_of_sexp

let of_sexp_string =
  Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.date_time_of_sexp

let pp_sexp = Printers.wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_date_time

module Zoneless = struct
  let to_sexp = To_sexp.sexp_of_zoneless

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.zoneless_of_sexp
end

module Time_zone = struct
  include Time_zone
end

module Time_zone_info = struct
  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.tz_info_of_sexp

  let to_sexp = To_sexp.sexp_of_tz_info
end
