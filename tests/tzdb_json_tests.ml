open Test_utils

module Alco = struct
  let tzdb_json_loads_correctly () =
    Alcotest.(check unit)
      "everything loads correctly"
      (List.iter
         (fun s ->
            let json_file_path = Filename.concat "tzdb-json" (s ^ ".json") in
            print_endline json_file_path;
            flush stdout;
            CCIO.with_in ~flags:[ Open_rdonly; Open_binary ] json_file_path
              (fun ic ->
                 let json_string = CCIO.read_all ic in
                 let tz_in_memory = Time_zone.make_exn s in
                 let tz_from_json =
                   CCResult.get_exn
                   @@ Time_zone.of_json_string json_string
                 in
                 assert (
                   Time_zone.equal
                     tz_in_memory
                     tz_from_json
                 )
              )
         )
         (Time_zone.available_time_zones))
      ()

  let suite =
    [
      Alcotest.test_case "tzdb_json_loads_correctly" `Quick
        tzdb_json_loads_correctly;
    ]
end
