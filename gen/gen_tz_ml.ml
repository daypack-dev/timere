type date_time = {
  year : int;
  month : int;
  day : int;
  hour : int;
  minute : int;
  second : int;
  tz_string : string;
}

type zdump_line = {
  date_time_utc : date_time;
  date_time_local : date_time;
  is_dst : bool;
  offset : int;
}

type transition = {
  start_utc : date_time;
  end_inc_utc : date_time option;
  tz_string : string;
  is_dst : bool;
  offset : int;
}

type tz_record = {
  orig_name : string list;
  normalized_name : string list;
  transitions : transition list;
}

let human_int_of_month s =
  match s with
  | "Jan" -> 1
  | "Feb" -> 2
  | "Mar" -> 3
  | "Apr" -> 4
  | "May" -> 5
  | "Jun" -> 6
  | "Jul" -> 7
  | "Aug" -> 8
  | "Sep" -> 9
  | "Oct" -> 10
  | "Nov" -> 11
  | "Dec" -> 12
  | _ -> failwith "Unexpected case"

module Parser = struct
  open MParser
  open Parser_components

  let date_time_p =
    non_space_string >> spaces1 >>
    alpha_string >>= (fun month ->
        let month = human_int_of_month month in
        spaces1 >> nat_zero >>= (fun day ->
            spaces1 >> nat_zero >>= (fun hour ->
                char ':' >> nat_zero >>= (fun minute ->
                    char ':' >> nat_zero >>= (fun second ->
                        spaces1 >> nat_zero >>= (fun year ->
                            spaces1 >> alpha_string |>> (fun tz_string ->
                                {year; month; day; hour; minute; second; tz_string}
                              )
                          )
                      )
                  )
              )
          )
      )

  let zdump_line =
    non_space_string >> spaces1 >>
    date_time_p >>= (fun date_time_utc ->
        assert (date_time_utc.tz_string = "UT");
        spaces1 >> char '=' >> spaces1 >> date_time_p >>= (fun date_time_local ->
            spaces1 >> string "isdst=" >> ((char '0' >>$ false) <|> (char '1' >>$ true)) >>= (fun is_dst ->
                spaces1 >> string "gmtoff=" >> (nat_zero <|> (char '-' >> nat_zero |>> fun x -> -x))
                |>> (fun offset ->
                    {date_time_utc; date_time_local; is_dst; offset}
                  )
              )
          )
      )
end

let parse_zdump_line (s : string) : zdump_line =
  MParser.parse_string Parser.zdump_line s ()
  |> Parser_components.result_of_mparser_result
  |> Result.get_ok

let transitions_of_zdump_lines (l : zdump_line list) : transition list =
  let rec aux acc l =
    match l with
    | [] -> List.rev acc
    | [x] ->
      aux ({ start_utc = x.date_time_utc; end_inc_utc = None; tz_string = x.date_time_local.tz_string; is_dst = x.is_dst; offset = x.offset} :: acc)
        []
    | x :: y :: rest ->
      assert (x.date_time_local.tz_string = y.date_time_local.tz_string);
      assert (x.is_dst = y.is_dst);
      assert (x.offset = y.offset);
      aux ({ start_utc = x.date_time_utc; end_inc_utc = Some (y.date_time_utc); tz_string = x.date_time_local.tz_string; is_dst = x.is_dst; offset = x.offset} :: acc)
        rest
  in
  let l =
    match l with
    | x :: rest when x.date_time_local.tz_string = "LMT" -> rest
    | _ -> l
  in
  aux [] l

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
  let zdump_lines =
    all_zoneinfo_file_paths
    |> List.map (fun s ->
        let ic = Unix.open_process_in (Printf.sprintf "zdump -V %s" s) in
        CCIO.read_lines_l ic
      )
    |> List.map (List.map parse_zdump_line)
  in
  let transitions =
    zdump_lines
    |> List.map (transitions_of_zdump_lines)
  in
  ()
