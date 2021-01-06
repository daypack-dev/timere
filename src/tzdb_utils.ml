let tz_table_of_json_string s : (Timere_tz_data.table, unit) result =
  let exception Invalid_data in
  try
    let json = Yojson.Basic.from_string s in
    match json with
    | `Assoc l ->
      let table_rows =
        match List.assoc "table" l with
        | `List l -> l
        | _ -> raise Invalid_data
      in
      let table =
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
              let entry = Timere_tz_data.{ is_dst; offset } in
              (start, entry)
            | _ -> raise Invalid_data)
        |> Array.of_list
      in
      Ok table
    | _ -> raise Invalid_data
  with _ -> Error ()
