open Test_utils

module Alco = struct
  let tzdb_json_loads_correctly () =
    Alcotest.(check unit)
      "everything loads correctly"
      (List.iter
         (fun s ->
            let json_file_path =
              Filename.concat "tzdb-json" (s ^ ".json")
            in
            print_endline json_file_path;
            flush stdout;
            CCIO.with_in ~flags:[ Open_rdonly; Open_binary ]
              json_file_path (fun ic ->
                  let json_string = CCIO.read_all ic in
                  let table_in_memory =
                    (Time_zone.make_exn s).record.table
                  in
                  let table_from_json =
                    (CCResult.get_exn @@ Tzdb_utils.tz_table_of_json_string json_string)
                  in
                  assert (
                    Array.length table_in_memory = Array.length table_from_json
                  );
                  assert (
                    CCArray.for_all2 (fun e1 e2 ->
                        e1 = e2
                      )
                      table_in_memory table_from_json
                  )
                )
         )
         (Time_zone.available_time_zones ())
      )
      ()

  let suite = [ Alcotest.test_case "tzdb_json_loads_correctly" `Quick tzdb_json_loads_correctly]
end
