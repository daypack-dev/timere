open Of_sexp_utils
open Date_time_components

let month_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match month_of_abbr_string s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "Failed to parse month: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for month: %s" (CCSexp.to_string x))

let weekday_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match weekday_of_abbr_string s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "Failed to parse weekday: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for weekday: %s" (CCSexp.to_string x))

let int_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      try int_of_string s
      with Failure _ ->
        invalid_data (Printf.sprintf "Failed to parse int: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for int: %s" (CCSexp.to_string x))

let ints_of_sexp_list (x : CCSexp.t) =
  match x with
  | `Atom _ ->
    invalid_data
      (Printf.sprintf "Expected list for ints: %s" (CCSexp.to_string x))
  | `List l -> List.map int_of_sexp l

let span_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom _ ->
    invalid_data
      (Printf.sprintf "Expected list for span: %s" (CCSexp.to_string x))
  | `List [ s; ns ] ->
    let s = int64_of_sexp s in
    let ns = int_of_sexp ns in
    Span.make ~s ~ns ()
  | `List _ ->
    invalid_data
      (Printf.sprintf "List too long for span: %s" (CCSexp.to_string x))

let tz_make_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match Time_zone.make s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "Unrecognized time zone: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for time zone: %s" (CCSexp.to_string x))

let tz_info_of_sexp (x : CCSexp.t) : Date_time_components.tz_info =
  match x with
  | `Atom _ ->
    invalid_data (Printf.sprintf "Invalid tz_info: %s" (CCSexp.to_string x))
  | `List l -> (
      match l with
      | [ x ] -> { tz = tz_make_of_sexp x; offset_from_utc = None }
      | [ x; offset_from_utc ] ->
        {
          tz = tz_make_of_sexp x;
          offset_from_utc =
            Some (Span.make ~s:(CCInt64.of_int @@ int_of_sexp offset_from_utc) ());
        }
      | _ ->
        invalid_data
          (Printf.sprintf "Invalid tz_info: %s" (CCSexp.to_string x)))

let date_time_of_sexp (x : CCSexp.t) =
  let invalid_data () =
    invalid_data (Printf.sprintf "Invalid date: %s" (CCSexp.to_string x))
  in
  match x with
  | `List [ year; day_of_year; hour; minute; second; ns; tz_info ] -> (
      let year = int_of_sexp year in
      let day_of_year = int_of_sexp day_of_year in
      let hour = int_of_sexp hour in
      let minute = int_of_sexp minute in
      let second = int_of_sexp second in
      let ns = int_of_sexp ns in
      let tz_info = tz_info_of_sexp tz_info in
      match tz_info with
      | { tz; offset_from_utc = None } -> (
          match
            Date_time.ISO_ord_date_time.make ~year ~day_of_year ~hour ~minute ~second ~ns ~tz
              ()
          with
          | Ok x -> x
          | Error _ -> invalid_data ())
      | { tz; offset_from_utc = Some tz_offset } -> (
          match
            Date_time.ISO_ord_date_time.make_unambiguous ~year ~day_of_year ~hour ~minute
              ~second ~ns ~tz ~tz_offset ()
          with
          | Ok x -> x
          | Error _ -> invalid_data ()))
  | _ -> invalid_data ()

let timestamp_of_sexp x =
  let dt = date_time_of_sexp x in
  match dt.tz_info with
  | { tz = _; offset_from_utc = None } ->
    invalid_data "Expected time zone offset 0, but got None instead"
  | { tz; offset_from_utc = Some tz_offset } -> (
      let tz_name = Time_zone.name tz in
      if tz_name <> "UTC" then
        invalid_data
          (Printf.sprintf "Expected time zone UTC, but got %s instead" tz_name)
      else if not Span.(equal tz_offset zero) then
        invalid_data "Expected time zone offset 0"
      else
        match Date_time.to_timestamp dt with
        | `Single x -> x
        | _ -> failwith "Unexpected case")
