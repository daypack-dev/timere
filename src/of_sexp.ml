open Of_sexp_utils

let month_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match Time.month_of_abbr_string s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "Failed to parse month: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for month: %s" (CCSexp.to_string x))

let weekday_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match Time.weekday_of_abbr_string s with
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
    Timedesc.Span.make ~s ~ns ()
  | `List _ ->
    invalid_data
      (Printf.sprintf "List too long for span: %s" (CCSexp.to_string x))

let tz_make_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match Timedesc.Time_zone.make s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "Unrecognized time zone: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for time zone: %s" (CCSexp.to_string x))

let tz_info_of_sexp x =
  match Timedesc.Time_zone_info.of_sexp x with
  | Ok x -> x
  | Error msg -> invalid_data msg

let date_time_of_sexp x =
  match Timedesc.of_sexp x with Ok x -> x | Error msg -> invalid_data msg

let timestamp_of_sexp x =
  match Timedesc.Timestamp.of_sexp x with
  | Ok x -> x
  | Error msg -> invalid_data msg

let range_of_sexp ~(f : CCSexp.t -> 'a) (x : CCSexp.t) =
  match x with
  | `List [ `Atom "range_inc"; x; y ] -> `Range_inc (f x, f y)
  | `List [ `Atom "range_exc"; x; y ] -> `Range_exc (f x, f y)
  | _ -> invalid_data (Printf.sprintf "Invalid range: %s" (CCSexp.to_string x))

let pattern_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom _ ->
    invalid_data
      (Printf.sprintf "Expected list for pattern: %s" (CCSexp.to_string x))
  | `List l -> (
      match l with
      | `Atom "pattern" :: l -> (
          let years, l =
            match l with
            | `List (`Atom "years" :: years) :: l ->
              (List.map int_of_sexp years, l)
            | _ -> ([], l)
          in
          let months, l =
            match l with
            | `List (`Atom "months" :: months) :: l ->
              (List.map month_of_sexp months, l)
            | _ -> ([], l)
          in
          let days, l =
            match l with
            | `List (`Atom "month_days" :: month_days) :: l ->
              (List.map int_of_sexp month_days, l)
            | _ -> ([], l)
          in
          let weekdays, l =
            match l with
            | `List (`Atom "weekdays" :: weekdays) :: l ->
              (List.map weekday_of_sexp weekdays, l)
            | _ -> ([], l)
          in
          let hours, l =
            match l with
            | `List (`Atom "hours" :: hours) :: l ->
              (List.map int_of_sexp hours, l)
            | _ -> ([], l)
          in
          let minutes, l =
            match l with
            | `List (`Atom "minutes" :: minutes) :: l ->
              (List.map int_of_sexp minutes, l)
            | _ -> ([], l)
          in
          let seconds, l =
            match l with
            | `List (`Atom "seconds" :: seconds) :: l ->
              (List.map int_of_sexp seconds, l)
            | _ -> ([], l)
          in
          match l with
          | [] ->
            Time.pattern ~years ~months ~days ~weekdays ~hours ~minutes
              ~seconds ()
          | _ ->
            invalid_data
              (Printf.sprintf "Invalid pattern: %s" (CCSexp.to_string x)))
      | _ ->
        invalid_data
          (Printf.sprintf "Invalid pattern: %s" (CCSexp.to_string x)))

let points_of_sexp (x : CCSexp.t) : Points.t =
  let open Points in
  let pick_of_sexp_list (l : CCSexp.t list) : pick =
    match l with
    | [ `Atom "s"; x ] -> S (int_of_sexp x)
    | [ `Atom "ms"; minute; second ] ->
      MS { minute = int_of_sexp minute; second = int_of_sexp second }
    | [ `Atom "hms"; hour; minute; second ] ->
      HMS
        {
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
        }
    | [ `Atom "whms"; weekday; hour; minute; second ] ->
      WHMS
        {
          weekday = weekday_of_sexp weekday;
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
        }
    | [ `Atom "dhms"; month_day; hour; minute; second ] ->
      DHMS
        {
          month_day = int_of_sexp month_day;
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
        }
    | [ `Atom "mdhms"; month; month_day; hour; minute; second ] ->
      MDHMS
        {
          month = month_of_sexp month;
          month_day = int_of_sexp month_day;
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
        }
    | [ `Atom "ymdhms"; year; month; month_day; hour; minute; second ] ->
      YMDHMS
        {
          year = int_of_sexp year;
          month = month_of_sexp month;
          month_day = int_of_sexp month_day;
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
        }
    | _ ->
      invalid_data (Printf.sprintf "Invalid points: %s" (CCSexp.to_string x))
  in
  match x with
  | `Atom _ ->
    invalid_data (Printf.sprintf "Invalid points: %s" (CCSexp.to_string x))
  | `List l -> (
      match l with
      | [ `Atom "points"; `List (`Atom "pick" :: pick) ] ->
        { pick = pick_of_sexp_list pick; tz_info = None }
      | [ `Atom "points"; `List (`Atom "pick" :: pick); tz_info ] ->
        {
          pick = pick_of_sexp_list pick;
          tz_info = Some (tz_info_of_sexp tz_info);
        }
      | _ ->
        invalid_data
          (Printf.sprintf "Invalid points: %s" (CCSexp.to_string x)))

let of_sexp (x : CCSexp.t) =
  let open Time in
  let rec aux x =
    match x with
    | `List l -> (
        match l with
        | [ `Atom "all" ] -> always
        | [ `Atom "empty" ] -> empty
        | `Atom "intervals" :: l ->
          l
          |> List.map (fun x ->
              match x with
              | `List [ x; y ] -> (timestamp_of_sexp x, timestamp_of_sexp y)
              | _ ->
                invalid_data
                  (Printf.sprintf "Expected list for interval: %s"
                     (CCSexp.to_string x)))
          |> sorted_intervals ~skip_invalid:false
        | `Atom "pattern" :: _ -> pattern_of_sexp x
        | [ `Atom "not"; x ] -> not (aux x)
        (* | [ `Atom "drop_points"; n; x ] -> drop_points (int_of_sexp n) (aux x)
         * | [ `Atom "take_points"; n; x ] -> take_points (int_of_sexp n) (aux x) *)
        | [ `Atom "shift"; n; x ] ->
          let n = span_of_sexp n in
          shift n (aux x)
        | [ `Atom "lengthen"; n; x ] ->
          let n = span_of_sexp n in
          lengthen n (aux x)
        | [ `Atom "with_tz"; n; x ] ->
          let tz = tz_make_of_sexp n in
          with_tz tz (aux x)
        | `Atom "inter" :: l -> inter (List.map aux l)
        | `Atom "union" :: l -> union (List.map aux l)
        | [ `Atom "bounded_intervals"; `Atom pick; bound; start; end_exc ] ->
          let pick =
            match pick with
            | "whole" -> `Whole
            | "snd" -> `Snd
            | _ -> invalid_data (Printf.sprintf "Invalid pick: %s" pick)
          in
          let bound = span_of_sexp bound in
          bounded_intervals pick bound (points_of_sexp start)
            (points_of_sexp end_exc)
        | [ `Atom "unchunk"; x ] -> aux_chunked CCFun.id x
        | _ ->
          invalid_data
            (Printf.sprintf "Invalid timere data: %s" (CCSexp.to_string x)))
    | `Atom _ ->
      invalid_data
        (Printf.sprintf "Expected list for timere data: %s"
           (CCSexp.to_string x))
  and aux_chunked f (x : CCSexp.t) : Time_ast.t =
    let open Infix in
    match x with
    | `List l -> (
        match l with
        | [ `Atom "chunk_disjoint_intervals"; x ] ->
          chunk `Disjoint_intervals f (aux x)
        | [ `Atom "chunk_at_year_boundary"; x ] ->
          chunk `At_year_boundary f (aux x)
        | [ `Atom "chunk_at_month_boundary"; x ] ->
          chunk `At_month_boundary f (aux x)
        | [ `Atom "chunk_by_duration"; span; `Atom "drop_partial"; x ] ->
          chunk (`By_duration_drop_partial (span_of_sexp span)) f (aux x)
        | [ `Atom "chunk_by_duration"; span; x ] ->
          chunk (`By_duration (span_of_sexp span)) f (aux x)
        | [ `Atom "drop"; n; chunked ] ->
          aux_chunked (drop (int_of_sexp n) %> f) chunked
        | [ `Atom "take"; n; chunked ] ->
          aux_chunked (take (int_of_sexp n) %> f) chunked
        | [ `Atom "take_nth"; n; chunked ] ->
          aux_chunked (take_nth (int_of_sexp n) %> f) chunked
        | [ `Atom "nth"; n; chunked ] ->
          aux_chunked (nth (int_of_sexp n) %> f) chunked
        | [
          `Atom "chunk_again";
          `List [ `Atom "chunk_disjoint_intervals"; chunked ];
        ] ->
          aux_chunked (chunk_again `Disjoint_intervals %> f) chunked
        | [
          `Atom "chunk_again"; `List [ `Atom "chunk_at_year_boundary"; chunked ];
        ] ->
          aux_chunked (chunk_again `At_year_boundary %> f) chunked
        | [
          `Atom "chunk_again"; `List [ `Atom "chunk_at_month_boundary"; chunked ];
        ] ->
          aux_chunked (chunk_again `At_month_boundary %> f) chunked
        | [
          `Atom "chunk_again";
          `List
            [ `Atom "chunk_by_duration"; span; `Atom "drop_partial"; chunked ];
        ] ->
          aux_chunked
            (chunk_again (`By_duration_drop_partial (span_of_sexp span)) %> f)
            chunked
        | [
          `Atom "chunk_again"; `List [ `Atom "chunk_by_duration"; span; chunked ];
        ] ->
          aux_chunked
            (chunk_again (`By_duration (span_of_sexp span)) %> f)
            chunked
        | _ ->
          invalid_data
            (Printf.sprintf "Invalid timere data: %s" (CCSexp.to_string x)))
    | `Atom _ ->
      invalid_data
        (Printf.sprintf "Expected list for timere data: %s"
           (CCSexp.to_string x))
  in
  aux x

let wrap_of_sexp (f : CCSexp.t -> 'a) : CCSexp.t -> ('a, string) result =
  fun x ->
  try Ok (f x) with
  | Invalid_data msg -> Error msg
  | Invalid_argument msg -> Error msg

let wrap_of_sexp_into_of_sexp_string (f : CCSexp.t -> 'a) :
  string -> ('a, string) result =
  fun s ->
  let res =
    try CCSexp.parse_string s
    with _ -> Error "Failed to parse string into sexp"
  in
  match res with Error msg -> Error msg | Ok x -> (wrap_of_sexp f) x

let of_sexp_string = wrap_of_sexp_into_of_sexp_string of_sexp
