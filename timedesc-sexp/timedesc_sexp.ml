let wrap_to_sexp_into_pp_sexp (f : 'a -> Sexplib.Sexp.t) :
  Format.formatter -> 'a -> unit =
  fun formatter x -> Sexplib.Sexp.pp formatter (f x)

module Date = struct
  let to_sexp = To_sexp.sexp_of_date

  let to_sexp_string x = Sexplib.Sexp.to_string (To_sexp.sexp_of_date x)

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.date_of_sexp

  let of_sexp_string =
    Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.date_of_sexp

  let pp_sexp = wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_date
end

module Time = struct
  let to_sexp = To_sexp.sexp_of_time

  let to_sexp_string x = Sexplib.Sexp.to_string (To_sexp.sexp_of_time x)

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.time_of_sexp

  let of_sexp_string =
    Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.time_of_sexp

  let pp_sexp = wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_time
end

module Span = struct
  let to_sexp = To_sexp.sexp_of_span

  let to_sexp_string x = Sexplib.Sexp.to_string (To_sexp.sexp_of_span x)

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.span_of_sexp

  let of_sexp_string =
    Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.span_of_sexp

  let pp_sexp = wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_span
end

module Timestamp = struct
  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.span_of_sexp

  let to_sexp = To_sexp.sexp_of_span
end

let to_sexp = To_sexp.sexp_of_date_time

let to_sexp_string x = Sexplib.Sexp.to_string (To_sexp.sexp_of_date_time x)

let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.date_time_of_sexp

let of_sexp_string =
  Of_sexp_utils.wrap_of_sexp_into_of_sexp_string Of_sexp.date_time_of_sexp

let pp_sexp = wrap_to_sexp_into_pp_sexp To_sexp.sexp_of_date_time

module Zoneless = struct
  let to_sexp = To_sexp.sexp_of_zoneless

  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.zoneless_of_sexp
end

module Time_zone = struct
  open Sexplib

  let of_sexp (x : Sexp.t) : Timedesc.Time_zone.t option =
    let open Of_sexp_utils in
    try
      match x with
      | List l -> (
          match l with
          | Atom "tz" :: Atom name :: transitions ->
            transitions
            |> List.map (fun x ->
                match x with
                | Sexp.List [ start; List [ Atom is_dst; offset ] ] ->
                  let start = int64_of_sexp start in
                  let is_dst =
                    match is_dst with
                    | "t" -> true
                    | "f" -> false
                    | _ -> invalid_data ""
                  in
                  let offset = int_of_sexp offset in
                  let entry = Timedesc.Time_zone.{ is_dst; offset } in
                  (start, entry)
                | _ -> invalid_data "")
            |> Timedesc.Time_zone.Raw.of_transitions ~name
          | _ -> invalid_data "")
      | Atom _ -> invalid_data ""
    with _ -> None

  let to_sexp (t : Timedesc.Time_zone.t) : Sexp.t =
    let open To_sexp_utils in
    Sexp.List
      (Atom "tz"
       :: Atom (Timedesc.Time_zone.name t)
       :: List.map
         (fun ((start, _), entry) ->
            Sexp.List
              [
                sexp_of_int64 start;
                Sexp.List
                  [
                    (if Timedesc.Time_zone.(entry.is_dst) then Atom "t" else Atom "f");
                    sexp_of_int entry.offset;
                  ];
              ])
         (Timedesc.Time_zone.Raw.to_transitions t))

  let of_string s =
    match Sexp.of_string s with
    | exception _ -> None
    | x -> of_sexp x

  module Db = struct
    open Sexplib

    let of_sexp (x : Sexp.t) : Timedesc.Time_zone.Db.db option =
      let open Of_sexp_utils in
      try
        match x with
        | Atom _ -> invalid_data ""
        | List l ->
          Some
            (l
             |> List.to_seq
             |> Seq.map (fun x ->
                 match of_sexp x with
                 | None -> invalid_data ""
                 | Some x -> x)
             |> Timedesc.Time_zone.Db.of_seq)
      with _ -> None

    let to_sexp db =
      Sexp.List
        (Timedesc_tzdb.M.bindings db
         |> List.map (fun (name, table) ->
             Timedesc.Time_zone.Raw.of_table_exn ~name table)
         |> List.map to_sexp
        )

    let of_string s =
      match Sexp.of_string s with
      | exception _ -> None
      | x -> of_sexp x
  end
end

module Time_zone_info = struct
  let of_sexp = Of_sexp_utils.wrap_of_sexp Of_sexp.tz_info_of_sexp

  let to_sexp = To_sexp.sexp_of_tz_info
end
