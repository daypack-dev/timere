open Timedesc.Time_zone

  let of_sexp (x : CCSexp.t) : t option =
    let open Of_sexp_utils in
    try
      match x with
      | `List l -> (
          match l with
          | `Atom "tz" :: `Atom name :: transitions ->
            transitions
            |> List.map (fun x ->
                match x with
                | `List [ start; `List [ `Atom is_dst; offset ] ] ->
                  let start = int64_of_sexp start in
                  let is_dst =
                    match is_dst with
                    | "t" -> true
                    | "f" -> false
                    | _ -> invalid_data ""
                  in
                  let offset = int_of_sexp offset in
                  let entry = { is_dst; offset } in
                  (start, entry)
                | _ -> invalid_data "")
            |> Raw.of_transitions ~name
          | _ -> invalid_data "")
      | `Atom _ -> invalid_data ""
    with _ -> None

  let to_sexp (t : t) : CCSexp.t =
    let open To_sexp_utils in
    CCSexp.(
      list
        (atom "tz"
         :: atom (name t)
         :: List.map
           (fun ((start, _), entry) ->
              list
                [
                  sexp_of_int64 start;
                  list
                    [
                      (if entry.is_dst then atom "t" else atom "f");
                      sexp_of_int entry.offset;
                    ];
                ])
           (Raw.to_transitions t)))

  let of_string s =
    let res =
      try CCSexp.parse_string s
      with _ -> Error "Failed to parse string into sexp"
    in
    match res with Error _ -> None | Ok x -> of_sexp x

    module Db = struct
      open Db

    let of_sexp (x : CCSexp.t) : db option =
      let open Of_sexp_utils in
      try
        match x with
        | `Atom _ -> invalid_data ""
        | `List l ->
          Some
            (l
             |> CCList.to_seq
             |> Seq.map (fun x ->
                 match of_sexp x with
                 | None -> invalid_data ""
                 | Some x -> x)
             |> of_seq)
      with _ -> None

    let to_sexp db =
      Timedesc_tzdb.M.bindings db
      |> List.map (fun (name, table) -> Timedesc.Time_zone.Raw.of_table_exn ~name table)
      |> List.map to_sexp
      |> CCSexp.list

    let of_string s =
      let res =
        try CCSexp.parse_string s
        with _ -> Error "Failed to parse string into sexp"
      in
      match res with Error _ -> None | Ok x -> of_sexp x
    end
