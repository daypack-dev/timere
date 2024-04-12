open Sexplib
open Of_sexp_utils
module T = Timedesc

let int_of_sexp (x : Sexp.t) =
  match x with
  | Atom s -> (
      try int_of_string s
      with Failure _ ->
        invalid_data (Printf.sprintf "failed to parse int: %s" s))
  | List _ ->
    invalid_data
      (Printf.sprintf "expected atom for int: %s" (Sexp.to_string x))

let ints_of_sexp_list (x : Sexp.t) =
  match x with
  | Atom _ ->
    invalid_data
      (Printf.sprintf "expected list for ints: %s" (Sexp.to_string x))
  | List l -> List.map int_of_sexp l

let span_of_sexp (x : Sexp.t) =
  match x with
  | Atom _ ->
    invalid_data
      (Printf.sprintf "expected list for span: %s" (Sexp.to_string x))
  | List [ s; ns ] ->
    let s = int64_of_sexp s in
    let ns = int_of_sexp ns in
    T.Span.make ~s ~ns ()
  | List _ ->
    invalid_data
      (Printf.sprintf "list too long for span: %s" (Sexp.to_string x))

let tz_make_of_sexp (x : Sexp.t) =
  match x with
  | Atom s -> (
      match T.Time_zone.make s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "unrecognized time zone: %s" s))
  | List _ ->
    invalid_data
      (Printf.sprintf "expected atom for time zone: %s" (Sexp.to_string x))

let tz_info_of_sexp (x : Sexp.t) : T.Time_zone_info.t =
  match x with
  | Atom _ ->
    invalid_data (Printf.sprintf "invalid tz_info: %s" (Sexp.to_string x))
  | List l -> (
      match l with
      | [ x ] -> T.Time_zone_info.make_exn ~tz:(tz_make_of_sexp x) ()
      | [ x; offset_from_utc ] ->
        T.Time_zone_info.make_exn 
          ~tz:(tz_make_of_sexp x)
          ~offset_from_utc:
            (T.Span.make
               ~s:(Int64.of_int @@ int_of_sexp offset_from_utc)
               ())
          ()
      | _ ->
        invalid_data
          (Printf.sprintf "invalid tz_info: %s" (Sexp.to_string x)))

let date_of_sexp (x : Sexp.t) =
  let invalid_data () =
    invalid_data (Printf.sprintf "invalid date: %s" (Sexp.to_string x))
  in
  match x with
  | List [ year; month; day ] -> (
      let year = int_of_sexp year in
      let month = int_of_sexp month in
      let day = int_of_sexp day in
      match T.Date.Ymd.make ~year ~month ~day with
      | Ok x -> x
      | Error _ -> invalid_data ())
  | _ -> invalid_data ()

let time_of_sexp (x : Sexp.t) =
  let invalid_data () =
    invalid_data (Printf.sprintf "invalid time: %s" (Sexp.to_string x))
  in
  match x with
  | List [ hour; minute; second; ns ] -> (
      let hour = int_of_sexp hour in
      let minute = int_of_sexp minute in
      let second = int_of_sexp second in
      let ns = int_of_sexp ns in
      match T.Time.make ~hour ~minute ~second ~ns () with
      | Ok x -> x
      | Error _ -> invalid_data ())
  | _ -> invalid_data ()

let zoneless_of_sexp (x : Sexp.t) =
  let invalid_data () =
    invalid_data (Printf.sprintf "Invalid zoneless: %s" (Sexp.to_string x))
  in
  match x with
  | List [ date; time ] ->
    let date = date_of_sexp date in
    let time = time_of_sexp time in
    T.Zoneless.of_date_and_time date time
  | _ -> invalid_data ()

let date_time_of_sexp (x : Sexp.t) =
  let invalid_data () =
    invalid_data (Printf.sprintf "invalid date time: %s" (Sexp.to_string x))
  in
  match x with
  | List [ date; time; tz; offset_from_utc ] ->
    let date = date_of_sexp date in
    let time = time_of_sexp time in
    let tz = tz_make_of_sexp tz in
    let offset_from_utc =
      match offset_from_utc with
      | List [ Atom "single"; offset ] ->
        `Single (T.Span.make_small ~s:(int_of_sexp offset) ())
      | List [ Atom "ambiguous"; offset1; offset2 ] ->
        let offset1 = T.Span.make_small ~s:(int_of_sexp offset1) () in
        let offset2 = T.Span.make_small ~s:(int_of_sexp offset2) () in
        `Ambiguous (offset1, offset2)
      | _ -> invalid_data ()
    in
    let dt =
      match offset_from_utc with
      | `Single offset_from_utc -> (
          match
            T.of_date_and_time_unambiguous ~tz ~offset_from_utc
              date time
          with
          | Ok x -> x
          | Error _ -> invalid_data ())
      | `Ambiguous _ -> (
          match
            T.of_date_and_time ~tz date time
          with
          | Ok x ->
            if
              T.equal_local_date_time_result T.Span.equal offset_from_utc
                T.(offset_from_utc x)
            then x
            else invalid_data ()
          | Error _ -> invalid_data ())
    in
    dt
  | _ -> invalid_data ()
