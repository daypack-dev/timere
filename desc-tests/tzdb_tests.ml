module Alco = struct
  let tzdb_make_all () =
    Alcotest.(check bool)
      "all tables can go through make"
      (List.for_all
         (fun s -> CCOpt.is_some @@ Timedesc.Time_zone.make s)
         Timedesc.Time_zone.available_time_zones)
      true

  let tzdb_jsons_load_correctly () =
    Alcotest.(check unit)
      "everything loads correctly"
      (List.iter
         (fun s ->
           let json_file_path = Filename.concat "../tzdb-json" (s ^ ".json") in
           print_endline json_file_path;
           flush stdout;
           CCIO.with_in ~flags:[ Open_rdonly; Open_binary ] json_file_path
             (fun ic ->
               let json_string = CCIO.read_all ic in
               let tz_in_memory = Timedesc.Time_zone.make_exn s in
               let tz_from_json =
                 CCOpt.get_exn_or
                   "Expected to load from JSON string successfully"
                 @@ Timedesc.Time_zone.JSON.of_string json_string
               in
               assert (Timedesc.Time_zone.equal tz_in_memory tz_from_json)))
         Timedesc.Time_zone.available_time_zones)
      ()

  let suite =
    [
      Alcotest.test_case "tzdb_make_all" `Quick tzdb_make_all;
      Alcotest.test_case "tzdb_jsons_load_correctly" `Quick
        tzdb_jsons_load_correctly;
    ]
end
