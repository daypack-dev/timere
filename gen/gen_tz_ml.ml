type sign =
  | Plus
  | Minus

type tz =
  | String of string
  | Offset of sign * int

type date_time = {
  year : int;
  month : int;
  day : int;
  hour : int;
  minute : int;
  second : int;
  tz : tz;
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
  tz : tz;
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

  let tz_p =
    attempt alpha_string
    |>> (fun s -> String s)
        <|> attempt
          ( char '+'
            >>$ Plus
                <|> (char '-' >>$ Minus)
            >>= fun sign ->
            digit
            >>= fun h1 ->
            digit
            >>= fun h2 ->
            let hour = int_of_string (Printf.sprintf "%c%c" h1 h2) in
            digit
            >>= (fun m1 ->
                digit
                |>> fun m2 ->
                let minute = int_of_string (Printf.sprintf "%c%c" m1 m2) in
                Offset (sign, (hour * 60) + minute))
                <|> return (Offset (sign, hour * 60)) )

  let date_time_p =
    non_space_string
    >> spaces1
    >> alpha_string
    >>= fun month ->
    let month = human_int_of_month month in
    spaces1
    >> nat_zero
    >>= fun day ->
    spaces1
    >> nat_zero
    >>= fun hour ->
    char ':'
    >> nat_zero
    >>= fun minute ->
    char ':'
    >> nat_zero
    >>= fun second ->
    spaces1
    >> nat_zero
    >>= fun year ->
    spaces1 >> tz_p |>> fun tz -> { year; month; day; hour; minute; second; tz }

  let zdump_line =
    non_space_string
    >> spaces1
    >> date_time_p
    >>= fun date_time_utc ->
    assert (date_time_utc.tz = String "UT");
    spaces1
    >> char '='
    >> spaces1
    >> date_time_p
    >>= fun date_time_local ->
    spaces1
    >> string "isdst="
    >> (char '0' >>$ false <|> (char '1' >>$ true))
    >>= fun is_dst ->
    spaces1
    >> string "gmtoff="
    >> (nat_zero <|> (char '-' >> nat_zero |>> fun x -> -x))
    |>> fun offset -> { date_time_utc; date_time_local; is_dst; offset }
end

let parse_zdump_line (s : string) : (zdump_line, string) result =
  MParser.parse_string Parser.zdump_line s ()
  |> Parser_components.result_of_mparser_result

let transitions_of_zdump_lines (l : zdump_line list) : transition list =
  let rec aux acc line_num l =
    match l with
    | [] -> List.rev acc
    | [ x ] ->
      aux
        ( {
          start_utc = x.date_time_utc;
          end_inc_utc = None;
          tz = x.date_time_local.tz;
          is_dst = x.is_dst;
          offset = x.offset;
        }
          :: acc )
        (succ line_num) []
    | x :: y :: rest ->
      if x.date_time_local.tz <> y.date_time_local.tz then
        failwith
          (Printf.sprintf
             "line: %d, local date times do not match in timezone" line_num)
      else (
        assert (x.is_dst = y.is_dst);
        assert (x.offset = y.offset);
        aux
          ( {
            start_utc = x.date_time_utc;
            end_inc_utc = Some y.date_time_utc;
            tz = x.date_time_local.tz;
            is_dst = x.is_dst;
            offset = x.offset;
          }
            :: acc )
          (succ line_num) rest )
  in
  let preprocess l =
    let rec aux line_num l =
      match l with
      | x :: y :: rest
        when x.date_time_local.tz = String "LMT"
          && y.date_time_local.tz = String "LMT" ->
        aux (succ line_num) rest
      | x :: y :: rest when x.date_time_local.tz <> y.date_time_local.tz ->
        aux (succ line_num) (y :: rest)
      | _ -> (line_num, l)
    in
    aux 0 l
  in
  l |> preprocess |> fun (line_num, l) -> aux [] line_num l

let gen () =
  let zoneinfo_file_dir = "/usr/share/zoneinfo/posix" in
  let all_zoneinfo_file_paths =
    FileUtil.(find Is_file zoneinfo_file_dir (fun x y -> y :: x) [])
  in
  let all_timezones =
    all_zoneinfo_file_paths
    |> List.map (fun s -> String.split_on_char '/' s)
    |> List.map (CCList.drop 5)
  in
  let zdump_lines =
    all_zoneinfo_file_paths
    |> List.map (fun s ->
        Printf.printf "Parsing zdump output of file:\n";
        Printf.printf "  %s\n" s;
        flush stdout;
        let ic = Unix.open_process_in (Printf.sprintf "zdump -V %s" s) in
        let lines = CCIO.read_lines_l ic in
        close_in ic;
        List.map
          (fun s ->
             match parse_zdump_line s with
             | Ok x -> x
             | Error msg ->
               failwith (Printf.sprintf "For line: %s, error: %s\n" s msg))
          lines)
  in
  let transitions =
    List.combine all_zoneinfo_file_paths zdump_lines
    |> List.map (fun (s, l) ->
        Printf.printf
          "Processing zdump output into transition table for file:\n";
        Printf.printf "  %s\n" s;
        flush stdout;
        transitions_of_zdump_lines l)
  in
  ()
