type date_time = {
  year : int;
  month : string;
  day : int;
  hour : int;
  minute : int;
  second : int;
}

type transition = {
  start_utc : date_time;
  end_inc_utc : date_time option;
  offset : int;
  tz_string : string;
}

type tz_record = {
  orig_name : string list;
  normalized_name : string list;
  transitions : transition list;
}

let gen () =
  let zoneinfo_file_dir =
    "/usr/share/zoneinfo/posix"
  in
  let all_zoneinfo_file_paths =
    FileUtil.(find Is_file zoneinfo_file_dir (fun x y -> y :: x) [])
  in
  let all_timezones =
    all_zoneinfo_file_paths
    |> List.map (fun s ->
        String.split_on_char '/' s
      )
    |> List.map (CCList.drop 5)
  in
  let zdump_outputs =
    all_zoneinfo_file_paths
    |> List.map (fun s ->
        let ic = Unix.open_process_in (Printf.sprintf "zdump -V %s" s) in
        CCIO.read_lines_l ic
      )
  in
  ()
