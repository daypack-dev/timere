include Time
module Time_zone = Time_zone

exception Invalid_format_string = Printers.Invalid_format_string

module Date_time = struct
  include Time.Date_time'

  type 'a local_result = 'a Time_zone.local_result

  let sprintf = Printers.sprintf_date_time

  let pp = Printers.pp_date_time

  let to_rfc3339 = RFC3339.of_date_time

  let of_iso8601 = ISO8601.to_date_time
end

module Duration = struct
  include Duration

  let sprint = Printers.sprint_duration

  let pp = Printers.pp_duration
end

type 'a range = 'a Range.range

type interval = Interval.t

module Infix = Infix

let resolve = Resolver.resolve

let sprintf_timestamp = Printers.sprintf_timestamp

let pp_timestamp = Printers.pp_timestamp

let sprintf_interval = Printers.sprintf_interval

let pp_interval = Printers.pp_interval

let to_sexp = To_sexp.to_sexp

let pp_sexp = Printers.pp_sexp

let to_sexp_string = To_sexp.to_sexp_string

let of_sexp = Of_sexp.of_sexp

let of_sexp_string = Of_sexp.of_sexp_string

module Utils = struct
  let flatten_month_ranges (months : month range Seq.t) :
    (month Seq.t, unit) CCResult.t =
    try Ok (Month_ranges.Flatten.flatten months)
    with Range.Range_is_invalid -> Error ()

  let flatten_month_range_list (months : month range list) :
    (month list, unit) CCResult.t =
    try Ok (Month_ranges.Flatten.flatten_list months)
    with Range.Range_is_invalid -> Error ()

  let flatten_month_day_ranges (month_days : int range Seq.t) :
    (int Seq.t, unit) CCResult.t =
    try Ok (Month_day_ranges.Flatten.flatten month_days)
    with Range.Range_is_invalid -> Error ()

  let flatten_month_day_range_list (month_days : int range list) :
    (int list, unit) CCResult.t =
    try Ok (Month_day_ranges.Flatten.flatten_list month_days)
    with Range.Range_is_invalid -> Error ()

  let flatten_weekday_ranges (weekdays : weekday range Seq.t) :
    (weekday Seq.t, unit) CCResult.t =
    try Ok (Weekday_ranges.Flatten.flatten weekdays)
    with Range.Range_is_invalid -> Error ()

  let flatten_weekday_range_list (weekdays : weekday range list) :
    (weekday list, unit) CCResult.t =
    try Ok (Weekday_ranges.Flatten.flatten_list weekdays)
    with Range.Range_is_invalid -> Error ()

  let tz_table_of_json_string s : (Timere_tz_data.table, unit) result =
    let exception Invalid_data in
    try
      let json = Yojson.Basic.from_string s in
      match json with
      | `Assoc l ->
        let table_rows = match List.assoc "table" l with
          | `List l -> l
          | _ -> raise Invalid_data
        in
        let table =
          table_rows
          |> List.map (fun row ->
              match row with
              | `List [`String s; `Assoc e] ->
                let start = Int64.of_string s in
                let is_dst =
                  match List.assoc "is_dst" e with
                  | `Bool b -> b
                  | _ -> raise Invalid_data
                in
                let offset =
                  match List.assoc "offset" e with
                  | `Int x -> x
                  | _ -> raise Invalid_data
                in
                let entry =
                  Timere_tz_data.{ is_dst; offset }
                in
                (start, entry)
              | _ -> raise Invalid_data)
          |> Array.of_list
        in
        Ok table
      | _ -> raise Invalid_data
    with
    | _ -> Error ()
end
