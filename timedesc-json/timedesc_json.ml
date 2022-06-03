open Timedesc

module Time_zone = struct
  open Time_zone

  let of_json json : t option =
    let exception Invalid_data in
    try
      match json with
      | `Assoc l ->
        let name =
          match List.assoc "name" l with
          | `String s -> s
          | _ -> raise Invalid_data
        in
        let table_rows =
          match List.assoc "table" l with
          | `List l -> l
          | _ -> raise Invalid_data
        in
        table_rows
        |> List.map (fun row ->
            match row with
            | `List [ `String s; `Assoc e ] ->
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
              let entry = { is_dst; offset } in
              (start, entry)
            | _ -> raise Invalid_data)
        |> Raw.of_transitions ~name
      | _ -> raise Invalid_data
    with _ -> None

  let of_string s = try of_json @@ Yojson.Basic.from_string s with _ -> None

  let to_json (t : t) : Yojson.Basic.t =
    `Assoc
      [
        ("name", `String (name t));
        ( "table",
          `List
            (Raw.to_transition_seq t
             |> Seq.map (fun ((start, _), entry) ->
                 `List
                   [
                     `String (Int64.to_string start);
                     `Assoc
                       [
                         ("is_dst", `Bool entry.is_dst);
                         ("offset", `Int entry.offset);
                       ];
                   ])
             |> List.of_seq) );
      ]
end
