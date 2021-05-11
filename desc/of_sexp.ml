open Of_sexp_utils

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

let tz_info_of_sexp (x : CCSexp.t) : Time_zone_info.t =
  match x with
  | `Atom _ ->
    invalid_data (Printf.sprintf "Invalid tz_info: %s" (CCSexp.to_string x))
  | `List l -> (
      match l with
      | [ x ] -> { tz = tz_make_of_sexp x; fixed_offset_from_utc = None }
      | [ x; offset_from_utc ] ->
        {
          tz = tz_make_of_sexp x;
          fixed_offset_from_utc =
            Some
              (Span.make
                 ~s:(CCInt64.of_int @@ int_of_sexp offset_from_utc)
                 ());
        }
      | _ ->
        invalid_data
          (Printf.sprintf "Invalid tz_info: %s" (CCSexp.to_string x)))

let date_time_of_sexp (x : CCSexp.t) =
  let invalid_data () =
    invalid_data (Printf.sprintf "Invalid date: %s" (CCSexp.to_string x))
  in
  match x with
  | `List [ year; month; day; hour; minute; second; ns; tz; offset_from_utc ] ->
    let year = int_of_sexp year in
    let month = int_of_sexp month in
    let day = int_of_sexp day in
    let hour = int_of_sexp hour in
    let minute = int_of_sexp minute in
    let second = int_of_sexp second in
    let ns = int_of_sexp ns in
    let tz = tz_make_of_sexp tz in
    let offset_from_utc =
      match offset_from_utc with
      | `List [ `Atom "single"; offset ] ->
        `Single (Span.make_small ~s:(int_of_sexp offset) ())
      | `List [ `Atom "ambiguous"; offset1; offset2 ] ->
        let offset1 = Span.make_small ~s:(int_of_sexp offset1) () in
        let offset2 = Span.make_small ~s:(int_of_sexp offset2) () in
        `Ambiguous (offset1, offset2)
      | _ -> invalid_data ()
    in
    let dt =
      match offset_from_utc with
      | `Single offset_from_utc -> (
          match
            Date_time.Ymd_date_time.make_unambiguous ~year ~month ~day ~hour
              ~minute ~second ~ns ~tz ~offset_from_utc ()
          with
          | Ok x -> x
          | Error _ -> invalid_data ())
      | `Ambiguous _ -> (
          match
            Date_time.Ymd_date_time.make ~year ~month ~day ~hour ~minute
              ~second ~ns ~tz ()
          with
          | Ok x ->
            if
              Date_time.equal_local_result ~eq:Span.equal offset_from_utc
                Date_time.(offset_from_utc x)
            then x
            else invalid_data ()
          | Error _ -> invalid_data ())
    in
    dt
  | _ -> invalid_data ()

let timestamp_of_sexp x =
  let dt = date_time_of_sexp x in
  match dt.offset_from_utc with
  | `Ambiguous _ ->
    invalid_data "Expected time zone offset 0, but got `Ambiguous instead"
  | `Single offset ->
    let tz = dt.tz in
    let tz_name = Time_zone.name tz in
    if tz_name <> "UTC" then
      invalid_data
        (Printf.sprintf "Expected time zone UTC, but got %s instead" tz_name)
    else if not Span.(equal offset zero) then
      invalid_data "Expected time zone offset 0"
    else Date_time.to_timestamp_single dt
