open Sexplib
open Of_sexp_utils

let month_of_sexp (x : Sexp.t) =
  match x with
  | Sexp.Atom s -> (
      match Time.month_of_abbr_string s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "failed to parse month: %s" s))
  | List _ ->
    invalid_data
      (Printf.sprintf "expected atom for month: %s" (Sexp.to_string x))

let weekday_of_sexp (x : Sexp.t) =
  match x with
  | Sexp.Atom s -> (
      match Time.weekday_of_abbr_string s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "failed to parse weekday: %s" s))
  | List _ ->
    invalid_data
      (Printf.sprintf "expected atom for weekday: %s" (Sexp.to_string x))

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

let span_of_sexp sexp =
  match Timedesc_sexp.Span.of_sexp sexp with
  | Ok x -> x
  | Error msg -> invalid_data msg

let tz_make_of_sexp (x : Sexp.t) =
  match x with
  | Atom s -> (
      match Timedesc.Time_zone.make s with
      | Some x -> x
      | None -> invalid_data (Printf.sprintf "unrecognized time zone: %s" s))
  | List _ ->
    invalid_data
      (Printf.sprintf "expected atom for time zone: %s" (Sexp.to_string x))

let tz_info_of_sexp x =
  match Timedesc_sexp.Time_zone_info.of_sexp x with
  | Ok x -> x
  | Error msg -> invalid_data msg

let date_time_of_sexp x =
  match Timedesc_sexp.of_sexp x with Ok x -> x | Error msg -> invalid_data msg

let timestamp_of_sexp x =
  match Timedesc_sexp.Timestamp.of_sexp x with
  | Ok x -> x
  | Error msg -> invalid_data msg

let range_of_sexp ~(f : Sexp.t -> 'a) (x : Sexp.t) =
  match x with
  | List [ Atom "range_inc"; x; y ] -> `Range_inc (f x, f y)
  | List [ Atom "range_exc"; x; y ] -> `Range_exc (f x, f y)
  | _ -> invalid_data (Printf.sprintf "invalid range: %s" (Sexp.to_string x))

let iso_week_pattern_of_sexp (x : Sexp.t) =
  match x with
  | Atom _ ->
    invalid_data
      (Printf.sprintf "expected list for ISO week pattern: %s" (Sexp.to_string x))
  | List l -> (
      match l with
      | [ Atom "iso_week_pattern"; List years; List weeks ]-> (
          let years = List.map int_of_sexp years in
          let weeks = List.map int_of_sexp weeks in
          Time.iso_week_pattern ~years ~weeks ()
        )
      | _ ->
        invalid_data
          (Printf.sprintf "invalid pattern: %s" (Sexp.to_string x))
    )

let pattern_of_sexp (x : Sexp.t) =
  match x with
  | Sexp.Atom _ ->
    invalid_data
      (Printf.sprintf "expected list for pattern: %s" (Sexp.to_string x))
  | Sexp.List l -> (
      match l with
      | Atom "pattern" :: l -> (
          let years, l =
            match l with
            | List (Atom "years" :: years) :: l ->
              (List.map int_of_sexp years, l)
            | _ -> ([], l)
          in
          let months, l =
            match l with
            | List (Atom "months" :: months) :: l ->
              (List.map month_of_sexp months, l)
            | _ -> ([], l)
          in
          let days, l =
            match l with
            | List (Atom "month_days" :: month_days) :: l ->
              (List.map int_of_sexp month_days, l)
            | _ -> ([], l)
          in
          let weekdays, l =
            match l with
            | List (Atom "weekdays" :: weekdays) :: l ->
              (List.map weekday_of_sexp weekdays, l)
            | _ -> ([], l)
          in
          let hours, l =
            match l with
            | List (Atom "hours" :: hours) :: l ->
              (List.map int_of_sexp hours, l)
            | _ -> ([], l)
          in
          let minutes, l =
            match l with
            | List (Atom "minutes" :: minutes) :: l ->
              (List.map int_of_sexp minutes, l)
            | _ -> ([], l)
          in
          let seconds, l =
            match l with
            | List (Atom "seconds" :: seconds) :: l ->
              (List.map int_of_sexp seconds, l)
            | _ -> ([], l)
          in
          let ns_ranges, l =
            match l with
            | List (Atom "ns" :: ns) :: l ->
              ( List.map
                  (fun l ->
                     match l with
                     | Sexp.List [ x; y ] ->
                       `Range_inc (int_of_sexp x, int_of_sexp y)
                     | _ ->
                       invalid_data
                         (Printf.sprintf "invalid pattern: %s"
                            (Sexp.to_string x)))
                  ns,
                l )
            | _ -> ([], l)
          in
          match l with
          | [] ->
            Time.pattern ~years ~months ~days ~weekdays ~hours ~minutes
              ~seconds ~ns_ranges ()
          | _ ->
            invalid_data
              (Printf.sprintf "invalid pattern: %s" (Sexp.to_string x)))
      | _ ->
        invalid_data
          (Printf.sprintf "invalid pattern: %s" (Sexp.to_string x)))

let points_of_sexp (x : Sexp.t) : Points.t =
  let open Points in
  let pick_of_sexp_list (l : Sexp.t list) : pick =
    match l with
    | [ Atom "n"; x ] -> N (int_of_sexp x)
    | [ Atom "sn"; second; ns ] ->
      SN { second = int_of_sexp second; ns = int_of_sexp ns }
    | [ Atom "msn"; minute; second; ns ] ->
      MSN
        {
          minute = int_of_sexp minute;
          second = int_of_sexp second;
          ns = int_of_sexp ns;
        }
    | [ Atom "hmsn"; hour; minute; second; ns ] ->
      HMSN
        {
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
          ns = int_of_sexp ns;
        }
    | [ Atom "whmsn"; weekday; hour; minute; second; ns ] ->
      WHMSN
        {
          weekday = weekday_of_sexp weekday;
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
          ns = int_of_sexp ns;
        }
    | [ Atom "dhmsn"; month_day; hour; minute; second; ns ] ->
      DHMSN
        {
          month_day = int_of_sexp month_day;
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
          ns = int_of_sexp ns;
        }
    | [ Atom "mdhmsn"; month; month_day; hour; minute; second; ns ] ->
      MDHMSN
        {
          month = month_of_sexp month;
          month_day = int_of_sexp month_day;
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
          ns = int_of_sexp ns;
        }
    | [ Atom "ymdhmsn"; year; month; month_day; hour; minute; second; ns ] ->
      YMDHMSN
        {
          year = int_of_sexp year;
          month = month_of_sexp month;
          month_day = int_of_sexp month_day;
          hour = int_of_sexp hour;
          minute = int_of_sexp minute;
          second = int_of_sexp second;
          ns = int_of_sexp ns;
        }
    | _ ->
      invalid_data (Printf.sprintf "invalid points: %s" (Sexp.to_string x))
  in
  match x with
  | Atom _ ->
    invalid_data (Printf.sprintf "invalid points: %s" (Sexp.to_string x))
  | List l -> (
      match l with
      | [ Atom "points"; List (Atom "pick" :: pick) ] ->
        { pick = pick_of_sexp_list pick; tz_info = None }
      | [ Atom "points"; List (Atom "pick" :: pick); tz_info ] ->
        {
          pick = pick_of_sexp_list pick;
          tz_info = Some (tz_info_of_sexp tz_info);
        }
      | _ ->
        invalid_data
          (Printf.sprintf "invalid points: %s" (Sexp.to_string x)))

let of_sexp (x : Sexp.t) =
  let open Time in
  let rec aux x =
    match x with
    | Sexp.List l -> (
        match l with
        | [ Atom "all" ] -> always
        | [ Atom "empty" ] -> empty
        | Atom "intervals" :: l ->
          l
          |> List.map (fun x ->
              match x with
              | Sexp.List [ x; y ] -> (timestamp_of_sexp x, timestamp_of_sexp y)
              | _ ->
                invalid_data
                  (Printf.sprintf "expected list for interval: %s"
                     (Sexp.to_string x)))
          |> sorted_intervals ~skip_invalid:false
        | Atom "iso_week_pattern" :: _ -> iso_week_pattern_of_sexp x
        | Atom "pattern" :: _ -> pattern_of_sexp x
        | [ Atom "not"; x ] -> not (aux x)
        | [ Atom "shift"; n; x ] ->
          let n = span_of_sexp n in
          shift n (aux x)
        | [ Atom "lengthen"; n; x ] ->
          let n = span_of_sexp n in
          lengthen n (aux x)
        | [ Atom "with_tz"; n; x ] ->
          let tz = tz_make_of_sexp n in
          with_tz tz (aux x)
        | Atom "inter" :: l -> inter (List.map aux l)
        | Atom "union" :: l -> union (List.map aux l)
        | [ Atom "pattern_intervals"; Atom mode; bound; start; end_ ] ->
          let inc_exc, mode =
            match mode with
            | "whole_inc" -> (Some `Inc, `Whole)
            | "whole_exc" -> (Some `Exc, `Whole)
            | "fst" -> (None, `Fst)
            | "snd" -> (None, `Snd)
            | _ -> invalid_data (Printf.sprintf "invalid mode: %s" mode)
          in
          let bound = span_of_sexp bound in
          pattern_intervals ?inc_exc ~bound mode (points_of_sexp start)
            (points_of_sexp end_)
        | [ Atom "unchunk"; x ] -> aux_chunked CCFun.id x
        | _ ->
          invalid_data
            (Printf.sprintf "invalid timere data: %s" (Sexp.to_string x)))
    | Atom _ ->
      invalid_data
        (Printf.sprintf "expected list for timere data: %s"
           (Sexp.to_string x))
  and aux_chunked f (x : Sexp.t) : Time_ast.t =
    let open Infix in
    match x with
    | List l -> (
        match l with
        | [ Atom "chunk_disjoint_intervals"; x ] ->
          chunk `Disjoint_intervals f (aux x)
        | [ Atom "chunk_at_year_boundary"; x ] ->
          chunk `At_year_boundary f (aux x)
        | [ Atom "chunk_at_month_boundary"; x ] ->
          chunk `At_month_boundary f (aux x)
        | [ Atom "chunk_by_duration"; span; Atom "drop_partial"; x ] ->
          chunk (`By_duration_drop_partial (span_of_sexp span)) f (aux x)
        | [ Atom "chunk_by_duration"; span; x ] ->
          chunk (`By_duration (span_of_sexp span)) f (aux x)
        | [ Atom "drop"; n; chunked ] ->
          aux_chunked (drop (int_of_sexp n) %> f) chunked
        | [ Atom "take"; n; chunked ] ->
          aux_chunked (take (int_of_sexp n) %> f) chunked
        | [ Atom "take_nth"; n; chunked ] ->
          aux_chunked (take_nth (int_of_sexp n) %> f) chunked
        | [ Atom "nth"; n; chunked ] ->
          aux_chunked (nth (int_of_sexp n) %> f) chunked
        | [
          Atom "chunk_again";
          List [ Atom "chunk_disjoint_intervals"; chunked ];
        ] ->
          aux_chunked (chunk_again `Disjoint_intervals %> f) chunked
        | [
          Atom "chunk_again"; List [ Atom "chunk_at_year_boundary"; chunked ];
        ] ->
          aux_chunked (chunk_again `At_year_boundary %> f) chunked
        | [
          Atom "chunk_again"; List [ Atom "chunk_at_month_boundary"; chunked ];
        ] ->
          aux_chunked (chunk_again `At_month_boundary %> f) chunked
        | [
          Atom "chunk_again";
          List
            [ Atom "chunk_by_duration"; span; Atom "drop_partial"; chunked ];
        ] ->
          aux_chunked
            (chunk_again (`By_duration_drop_partial (span_of_sexp span)) %> f)
            chunked
        | [
          Atom "chunk_again"; List [ Atom "chunk_by_duration"; span; chunked ];
        ] ->
          aux_chunked
            (chunk_again (`By_duration (span_of_sexp span)) %> f)
            chunked
        | _ ->
          invalid_data
            (Printf.sprintf "invalid timere data: %s" (Sexp.to_string x)))
    | Atom _ ->
      invalid_data
        (Printf.sprintf "expected list for timere data: %s"
           (Sexp.to_string x))
  in
  aux x

let of_sexp_string = wrap_of_sexp_into_of_sexp_string of_sexp
