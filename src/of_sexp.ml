exception Invalid_data of string

let invalid_data s = raise (Invalid_data s)

let month_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match Time.month_of_abbr_string s with
      | Ok x -> x
      | Error () -> invalid_data (Printf.sprintf "Failed to parse month: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for month: %s" (CCSexp.to_string x))

let weekday_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match Time.weekday_of_abbr_string s with
      | Ok x -> x
      | Error () ->
        invalid_data (Printf.sprintf "Failed to parse weekday: %s" s))
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

let int64_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      try Int64.of_string s
      with Failure _ ->
        invalid_data (Printf.sprintf "Failed to parse int64: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for int64: %s" (CCSexp.to_string x))

let ints_of_sexp_list (x : CCSexp.t) =
  match x with
  | `Atom _ ->
    invalid_data
      (Printf.sprintf "Expected list for ints: %s" (CCSexp.to_string x))
  | `List l -> List.map int_of_sexp l

let tz_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      match Time_zone.make s with
      | Ok x -> x
      | Error () -> invalid_data (Printf.sprintf "Unrecognized time zone: %s" s)
    )
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for time zone: %s" (CCSexp.to_string x))

let duration_of_sexp (x : CCSexp.t) =
  match x with
  | `List [ days; hours; minutes; seconds ] ->
    let days = int_of_sexp days in
    let hours = int_of_sexp hours in
    let minutes = int_of_sexp minutes in
    let seconds = int_of_sexp seconds in
    Duration.make ~days ~hours ~minutes ~seconds ()
  | _ ->
    invalid_data (Printf.sprintf "Invalid duration: %s" (CCSexp.to_string x))

let date_time_of_sexp (x : CCSexp.t) =
  let invalid_data () =
    invalid_data (Printf.sprintf "Invalid date: %s" (CCSexp.to_string x))
  in
  match x with
  | `List [ year; month; day; hour; minute; second; `List tz_info ] -> (
      let year = int_of_sexp year in
      let month = month_of_sexp month in
      let day = int_of_sexp day in
      let hour = int_of_sexp hour in
      let minute = int_of_sexp minute in
      let second = int_of_sexp second in
      let tz_info =
        match tz_info with
        | [ `Atom "tz"; x ] -> `Tz_only (tz_of_sexp x)
        | [ `Atom "tz_offset_s"; x ] -> `Tz_offset_s_only (int_of_sexp x)
        | [ `Atom "tz_and_tz_offset_s"; tz; tz_offset_s ] ->
          `Tz_and_tz_offset_s (tz_of_sexp tz, int_of_sexp tz_offset_s)
        | _ -> invalid_data ()
      in
      match tz_info with
      | `Tz_only tz -> (
          match
            Time.Date_time.make ~year ~month ~day ~hour ~minute ~second ~tz
          with
          | Ok x -> x
          | Error () -> invalid_data ())
      | `Tz_offset_s_only tz_offset_s -> (
          match
            Time.Date_time.make_precise ~year ~month ~day ~hour ~minute ~second
              ~tz_offset_s ()
          with
          | Ok x -> x
          | Error () -> invalid_data ())
      | `Tz_and_tz_offset_s (tz, tz_offset_s) -> (
          match
            Time.Date_time.make_precise ~tz ~year ~month ~day ~hour ~minute
              ~second ~tz_offset_s ()
          with
          | Ok x -> x
          | Error () -> invalid_data ()))
  | _ -> invalid_data ()

let timestamp_of_sexp x =
  let dt = date_time_of_sexp x in
  match dt.tz_info with
  | `Tz_only _ ->
    invalid_data "Expected time zone offset 0, but got None instead"
  | `Tz_offset_s_only _ ->
    invalid_data "Expected time zone UTC, but got None instead"
  | `Tz_and_tz_offset_s (tz, tz_offset_s) -> (
      let tz_name = Time_zone.name tz in
      if tz_name <> "UTC" then
        invalid_data
          (Printf.sprintf "Expected time zone UTC, but got %s instead" tz_name)
      else if tz_offset_s <> 0 then
        invalid_data
          (Printf.sprintf "Expected time zone offset 0, but got %d instead"
             tz_offset_s)
      else
        match Time.Date_time.to_timestamp dt with
        | `Exact x -> x
        | _ -> failwith "Unexpected case")

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
          let month_days, l =
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
            Time.pattern ~strict:false ~years ~months ~month_days ~weekdays
              ~hours ~minutes ~seconds ()
          | _ ->
            invalid_data
              (Printf.sprintf "Invalid pattern: %s" (CCSexp.to_string x)))
      | _ ->
        invalid_data
          (Printf.sprintf "Invalid pattern: %s" (CCSexp.to_string x)))

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
          |> of_sorted_intervals ~skip_invalid:false
        | `Atom "pattern" :: _ -> pattern_of_sexp x
        | [ `Atom "not"; x ] -> not (aux x)
        | [ `Atom "every"; x ] -> aux x
        | [ `Atom "drop_n_points"; n; x ] ->
          drop_n_points (int_of_sexp n) (aux x)
        | [ `Atom "take_n_points"; n; x ] ->
          take_n_points (int_of_sexp n) (aux x)
        | [ `Atom "shift"; n; x ] ->
          let n = Duration.of_seconds (int64_of_sexp n) in
          shift n (aux x)
        | [ `Atom "lengthen"; n; x ] ->
          let n = Duration.of_seconds (int64_of_sexp n) in
          lengthen n (aux x)
        | [ `Atom "with_tz"; n; x ] ->
          let tz = tz_of_sexp n in
          with_tz tz (aux x)
        | [ `Atom "interval_inc"; a; b ] ->
          interval_dt_inc (date_time_of_sexp a) (date_time_of_sexp b)
        | [ `Atom "interval_exc"; a; b ] ->
          interval_dt_exc (date_time_of_sexp a) (date_time_of_sexp b)
        | `Atom "round_robin" :: l -> round_robin_pick (List.map aux l)
        | `Atom "inter" :: l -> inter (List.map aux l)
        | `Atom "union" :: l -> union (List.map aux l)
        | [ `Atom "after"; b; t1; t2 ] ->
          after (duration_of_sexp b) (aux t1) (aux t2)
        | [ `Atom "between_inc"; b; t1; t2 ] ->
          between_inc (duration_of_sexp b) (aux t1) (aux t2)
        | [ `Atom "between_exc"; b; t1; t2 ] ->
          between_exc (duration_of_sexp b) (aux t1) (aux t2)
        | [ `Atom "unchunk"; x ] -> aux_chunked (fun x -> x) x
        | _ ->
          invalid_data
            (Printf.sprintf "Invalid timere data: %s" (CCSexp.to_string x)))
    | `Atom _ ->
      invalid_data
        (Printf.sprintf "Expected list for timere data: %s"
           (CCSexp.to_string x))
  and aux_chunked f (x : CCSexp.t) : t =
    match x with
    | `List l -> (
        match l with
        | [ `Atom "chunk_disjoint_intervals"; x ] ->
          chunk `Disjoint_intervals f (aux x)
        | [ `Atom "chunk_at_year_boundary"; x ] ->
          chunk `At_year_boundary f (aux x)
        | [ `Atom "chunk_at_month_boundary"; x ] ->
          chunk `At_month_boundary f (aux x)
        | [ `Atom "chunk_by_duration"; duration; `Atom "drop_partial"; x ] ->
          chunk
            (`By_duration_drop_partial (duration_of_sexp duration))
            f (aux x)
        | [ `Atom "chunk_by_duration"; duration; x ] ->
          chunk (`By_duration (duration_of_sexp duration)) f (aux x)
        | [ `Atom "drop"; n; chunked ] ->
          aux_chunked (fun x -> x |> drop (int_of_sexp n) |> f) chunked
        | [ `Atom "take"; n; chunked ] ->
          aux_chunked (fun x -> x |> take (int_of_sexp n) |> f) chunked
        | [ `Atom "take_nth"; n; chunked ] ->
          aux_chunked (fun x -> x |> take_nth (int_of_sexp n) |> f) chunked
        | [ `Atom "nth"; n; chunked ] ->
          aux_chunked (fun x -> x |> nth (int_of_sexp n) |> f) chunked
        | [
          `Atom "chunk_again";
          `List [ `Atom "chunk_disjoint_intervals"; chunked ];
        ] ->
          aux_chunked
            (fun x -> x |> chunk_again `Disjoint_intervals |> f)
            chunked
        | [
          `Atom "chunk_again"; `List [ `Atom "chunk_at_year_boundary"; chunked ];
        ] ->
          aux_chunked
            (fun x -> x |> chunk_again `At_year_boundary |> f)
            chunked
        | [
          `Atom "chunk_again"; `List [ `Atom "chunk_at_month_boundary"; chunked ];
        ] ->
          aux_chunked
            (fun x -> x |> chunk_again `At_month_boundary |> f)
            chunked
        | [
          `Atom "chunk_again";
          `List
            [
              `Atom "chunk_by_duration"; duration; `Atom "drop_partial"; chunked;
            ];
        ] ->
          aux_chunked
            (fun x ->
               x
               |> chunk_again
                 (`By_duration_drop_partial (duration_of_sexp duration))
               |> f)
            chunked
        | [
          `Atom "chunk_again";
          `List [ `Atom "chunk_by_duration"; duration; chunked ];
        ] ->
          aux_chunked
            (fun x ->
               x |> chunk_again (`By_duration (duration_of_sexp duration)) |> f)
            chunked
        | _ ->
          invalid_data
            (Printf.sprintf "Invalid timere data: %s" (CCSexp.to_string x)))
    | `Atom _ ->
      invalid_data
        (Printf.sprintf "Expected list for timere data: %s"
           (CCSexp.to_string x))
  in
  try Ok (aux x) with
  | Invalid_data msg -> Error msg
  | Invalid_argument msg -> Error msg

let of_sexp_string s =
  let res =
    try CCSexp.parse_string s
    with _ -> Error "Failed to parse string into sexp"
  in
  match res with Error msg -> Error msg | Ok x -> of_sexp x
