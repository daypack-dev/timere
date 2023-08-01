open Sexplib

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
  start_local : date_time;
  end_inc_local : date_time option;
  tz : tz;
  is_dst : bool;
  offset : int;
}

type transition_record = {
  start : int64;
  end_exc : int64;
  tz : tz;
  is_dst : bool;
  offset : int;
}

type transition_table = string * transition_record list

let year_start_fallback = 1850

let year_start = int_of_string Sys.argv.(1)

let year_end_exc = int_of_string Sys.argv.(2)

let output_name_suffix = Sys.argv.(3)

let array_literal_max_size = 200

let self_dir = "gen/"

let output_dir = "gen-artifacts/"

let output_flags = [ Open_wronly; Open_creat; Open_trunc; Open_binary ]

let output_list_file_name = output_dir ^ "available-time-zones.txt"

let sexp_output_file_name =
  Printf.sprintf "%stzdb_%s.sexp" output_dir output_name_suffix

let compressed_output_file_name =
  Printf.sprintf "%stzdb_compressed_%s.ml" output_dir output_name_suffix

let tz_constants_file_name = output_dir ^ "time_zone_constants.ml"

let tzdb_json_output_dir = "tzdb-json/"

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
  open Angstrom
  open Parser_components

  let tz_p =
    alpha_string
    >>| (fun s -> String s)
        <|>
        ((char '+' *> return Plus)
         <|> (char '-' *> return Minus)
         >>= fun sign ->
         digit
         >>= fun h1 ->
         digit
         >>= fun h2 ->
         let hour = int_of_string (Printf.sprintf "%c%c" h1 h2) in
         (digit
          >>= (fun m1 ->
              digit
              >>| fun m2 ->
              let minute = int_of_string (Printf.sprintf "%c%c" m1 m2) in
              Offset (sign, (hour * 60) + minute)))
         <|> return (Offset (sign, hour * 60)))

  let date_time_p =
    non_space_string
    *> spaces1
    *> alpha_string
    >>= fun month ->
    let month = human_int_of_month month in
    spaces1
    *> nat_zero
    >>= fun day ->
    spaces1
    *> nat_zero
    >>= fun hour ->
    char ':'
    *> nat_zero
    >>= fun minute ->
    char ':'
    *> nat_zero
    >>= fun second ->
    spaces1
    *> nat_zero
    >>= fun year ->
    spaces1 *> tz_p >>| fun tz -> { year; month; day; hour; minute; second; tz }

  let zdump_line =
    non_space_string
    *> spaces1
    *> date_time_p
    >>= fun date_time_utc ->
    assert (date_time_utc.tz = String "UT");
    spaces1
    *> char '='
    *> spaces1
    *> date_time_p
    >>= fun date_time_local ->
    spaces1
    *> string "isdst="
    *> ((char '0' *> return false) <|> (char '1' *> return true))
    >>= fun is_dst ->
    spaces1
    *> string "gmtoff="
    *> (nat_zero <|> (char '-' *> nat_zero >>| fun x -> -x))
    >>| fun offset -> { date_time_utc; date_time_local; is_dst; offset }
end

let parse_zdump_line (s : string) : (zdump_line, string) result =
  Angstrom.(parse_string ~consume:All Parser.zdump_line s)

let transitions_of_zdump_lines (l : zdump_line list) : transition list =
  let rec aux acc line_num l =
    match l with
    | [] -> List.rev acc
    | [ x ] ->
      aux
        ({
          start_utc = x.date_time_utc;
          end_inc_utc = None;
          start_local = x.date_time_local;
          end_inc_local = None;
          tz = x.date_time_local.tz;
          is_dst = x.is_dst;
          offset = x.offset;
        }
          :: acc)
        (succ line_num) []
    | x :: y :: rest ->
      if x.date_time_local.tz <> y.date_time_local.tz then
        failwith
          (Printf.sprintf
             "line: %d, local date times do not match in time zone" line_num)
      else (
        assert (x.is_dst = y.is_dst);
        assert (x.offset = y.offset);
        aux
          ({
            start_utc = x.date_time_utc;
            end_inc_utc = Some y.date_time_utc;
            start_local = x.date_time_utc;
            end_inc_local = Some y.date_time_utc;
            tz = x.date_time_local.tz;
            is_dst = x.is_dst;
            offset = x.offset;
          }
            :: acc)
          (succ line_num) rest)
  in
  let preprocess l =
    let rec aux line_num l =
      match l with
      | x :: y :: rest
        when x.date_time_local.tz = String "LMT"
          && y.date_time_local.tz = String "LMT" ->
        aux (succ line_num) rest
      | x :: y :: rest
        when x.date_time_local.tz <> y.date_time_local.tz
          || x.offset <> y.offset ->
        aux (succ line_num) (y :: rest)
      | _ -> (line_num, l)
    in
    aux 0 l
  in
  l |> preprocess |> fun (line_num, l) -> aux [] line_num l

let min_timestamp =
  Timedesc.Span.get_s (Timedesc.Utils.timestamp_of_ptime Ptime.min)

let max_timestamp =
  Timedesc.Span.get_s (Timedesc.Utils.timestamp_of_ptime Ptime.max)
  |> Int64.pred

let timestamp_of_date_time_utc (x : date_time) : int64 =
  assert (x.tz = String "UT");
  let offset = 0 in
  Ptime.of_date_time
    ((x.year, x.month, x.day), ((x.hour, x.minute, x.second), offset))
  |> CCOption.get_exn_or "Expected successful construction of ptime"
  |> Timedesc.Utils.timestamp_of_ptime
  |> Timedesc.Span.get_s

let timestamp_of_date_time_local (x : date_time) : int64 =
  let offset = 0 in
  Ptime.of_date_time
    ((x.year, x.month, x.day), ((x.hour, x.minute, x.second), offset))
  |> CCOption.get_exn_or "Expected successful construction of ptime"
  |> Timedesc.Utils.timestamp_of_ptime
  |> Timedesc.Span.get_s

let transition_record_indexed_by_utc_of_transition (x : transition) :
  transition_record =
  let start = timestamp_of_date_time_utc x.start_utc in
  let end_exc =
    match x.end_inc_utc with
    | None -> max_timestamp
    | Some end_inc_utc -> timestamp_of_date_time_utc end_inc_utc |> Int64.succ
  in
  { start; end_exc; tz = x.tz; is_dst = x.is_dst; offset = x.offset }

let transition_record_indexed_by_local_of_transition (x : transition) :
  transition_record =
  let start = timestamp_of_date_time_local x.start_local in
  let end_exc =
    match x.end_inc_local with
    | None -> max_timestamp
    | Some end_inc_local ->
      timestamp_of_date_time_local end_inc_local |> Int64.succ
  in
  { start; end_exc; tz = x.tz; is_dst = x.is_dst; offset = x.offset }

let check_transition_records_are_contiguous (l : transition_record list) :
  transition_record list =
  let rec aux l =
    match l with
    | [] | [ _ ] -> l
    | x :: y :: xs ->
      if x.end_exc = y.start then x :: aux (y :: xs)
      else failwith "Transition records are not contiguous"
  in
  aux l

let process_overlapping_transition_records (l : transition_record list) :
  transition_record list =
  let rec aux l =
    match l with
    | [] | [ _ ] -> l
    | x :: y :: xs ->
      if y.start < x.end_exc then
        let z1 = { x with start = x.start; end_exc = y.start } in
        let z2 = { x with start = y.start; end_exc = x.end_exc } in
        let z3 = { y with start = y.start; end_exc = x.end_exc } in
        let z4 = { y with start = x.end_exc; end_exc = y.end_exc } in
        z1 :: z2 :: z3 :: aux (z4 :: xs)
      else x :: aux (y :: xs)
  in
  aux l

let () =
  let zoneinfo_file_dir = "/usr/share/zoneinfo/posix" in
  let all_zoneinfo_file_paths =
    FileUtil.(find ~follow:Follow Is_file zoneinfo_file_dir (fun x y -> y :: x) [])
    |> List.sort_uniq String.compare
  in
  let all_time_zones_in_parts =
    all_zoneinfo_file_paths
    |> List.map (fun s -> String.split_on_char '/' s)
    |> List.map (CCList.drop 5)
  in
  let all_time_zones =
    all_time_zones_in_parts |> List.map (String.concat "/")
  in
  let zdump_lines =
    all_zoneinfo_file_paths
    |> List.map (fun s ->
        Printf.printf "Parsing zdump output of file:\n";
        Printf.printf "  %s\n" s;
        flush stdout;
        let ic =
          Unix.open_process_in
            (Printf.sprintf "zdump -V -c %d,%d %s" year_start year_end_exc s)
        in
        let lines_first_try = CCIO.read_lines_l ic in
        close_in ic;
        let lines =
          match lines_first_try with
          | [] ->
            let ic =
              Unix.open_process_in
                (Printf.sprintf "zdump -V -c %d,%d %s" year_start_fallback year_end_exc s)
            in
            let lines = CCIO.read_lines_l ic in
            close_in ic;
            lines
          | l -> l
        in
        List.map
          (fun s ->
             match parse_zdump_line s with
             | Ok x -> x
             | Error msg ->
               failwith (Printf.sprintf "For line: %s, error: %s\n" s msg))
          lines)
  in
  print_newline ();
  let transitions =
    List.combine all_zoneinfo_file_paths zdump_lines
    |> List.map (fun (s, l) ->
        Printf.printf "Processing zdump output into transitions for file:\n";
        Printf.printf "  %s\n" s;
        flush stdout;
        transitions_of_zdump_lines l)
  in
  print_newline ();
  let tables_utc : transition_table list =
    List.combine all_time_zones transitions
    |> CCList.to_seq
    |> Seq.map (fun (s, l) ->
        Printf.printf "Constructing transition table for time zone: %s\n%!" s;
        let l =
          l
          |> List.map transition_record_indexed_by_utc_of_transition
          |> check_transition_records_are_contiguous
        in
        (s, l))
    |> Seq.map (fun (s, l) ->
        let l =
          match l with
          | [] -> (
              let base =
                {
                  start = min_timestamp;
                  end_exc = max_timestamp;
                  tz = String s;
                  is_dst = false;
                  offset = 0;
                }
              in
              match s with
              | "UTC" | "UCT" | "GMT" | "GMT-0" | "GMT+0" | "GMT0"
              | "Universal" | "Greenwich" | "Zulu" | "Factory" | "Etc/GMT"
              | "Etc/GMT0" | "Etc/UTC" | "Etc/UCT" | "Etc/Universal"
              | "Etc/Greenwich" | "Etc/Zulu" ->
                [ base ]
              | "EST" -> [ { base with offset = -5 * 60 * 60 } ]
              | "HST" -> [ { base with offset = -10 * 60 * 60 } ]
              | "MST" -> [ { base with offset = -7 * 60 * 60 } ]
              | s -> (
                  try
                    Scanf.sscanf s "Etc/GMT%d" (fun x ->
                        [ { base with offset = x * 60 * 60 } ])
                  with _ ->
                    failwith
                      (Printf.sprintf
                         "Unrecognized time zone during special case \
                          handling: %s"
                         s)))
          | x :: _ ->
            if x.start <> min_timestamp then
              let filler =
                {
                  start = min_timestamp;
                  end_exc = x.start;
                  tz = x.tz;
                  is_dst = x.is_dst;
                  offset = x.offset;
                }
              in
              filler :: l
            else l
        in
        (s, l))
    |> CCList.of_seq
  in
  print_newline ();
  Printf.printf "Number of time zones: %d\n" (List.length all_time_zones);
  print_newline ();
  Printf.printf "Maximum number of records: %d\n"
    (CCList.fold_left (fun x (_, l) -> max (List.length l) x) 0 tables_utc);
  print_newline ();
  FileUtil.mkdir ~parent:true output_dir;
  Printf.printf "Generating %s\n" output_list_file_name;
  CCIO.with_out ~flags:output_flags output_list_file_name (fun oc ->
      CCIO.write_lines_l oc all_time_zones);

  Printf.printf "Constructing tzdb\n";
  let time_zones : Timedesc.Time_zone.t list =
    List.map
      (fun (name, l) ->
         let transitions =
           List.map
             (fun (r : transition_record) ->
                ( r.start,
                  { Timedesc.Time_zone.is_dst = r.is_dst; offset = r.offset } ))
             l
         in
         CCOption.get_exn_or
           "Expected successful construction of time zone from transitions"
         @@ Timedesc.Time_zone.Raw.of_transitions ~name transitions)
      tables_utc
  in
  let db = Timedesc.Time_zone.Db.of_seq @@ CCList.to_seq time_zones in

  Printf.printf "Generating sexp serialization of tzdb: %s\n" sexp_output_file_name;
  CCIO.with_out ~flags:output_flags sexp_output_file_name (fun oc ->
      Format.fprintf (CCFormat.of_chan oc) "%a@." Sexp.pp
        (Timedesc_sexp.Time_zone.Db.to_sexp db));

  Printf.printf "Generating compressed serialization of tzdb: %s\n" compressed_output_file_name;
  CCIO.with_out ~flags:output_flags compressed_output_file_name (fun oc ->
      Format.fprintf (CCFormat.of_chan oc) "let s = %S@."
        (Timedesc.Time_zone.Db.Compressed.to_string db));

  Printf.printf "Generating %s\n" tz_constants_file_name;
  CCIO.with_out ~flags:output_flags tz_constants_file_name (fun oc ->
      let walk f start =
        List.fold_left
          (fun pick (_, transitions) ->
             List.fold_left
               (fun pick (r : transition_record) -> f r pick)
               pick transitions)
          start tables_utc
      in
      let greatest_neg_tz_offset_s, greatest_pos_tz_offset_s =
        walk
          (fun r (low, high) -> (min r.offset low, max r.offset high))
          (max_int, min_int)
      in
      let greatest_neg_tz_offset_s = abs greatest_neg_tz_offset_s in
      Printf.fprintf oc
        {|
let greatest_neg_tz_offset_s = %d
let greatest_pos_tz_offset_s = %d
|}
        greatest_neg_tz_offset_s greatest_pos_tz_offset_s);

  print_endline "Generating tzdb JSON";
  List.combine all_time_zones_in_parts time_zones
  |> List.iter (fun (time_zone_parts, tz) ->
      let len = List.length time_zone_parts in
      assert (len > 0);
      let dir_parts =
        tzdb_json_output_dir :: CCList.take (len - 1) time_zone_parts
      in
      let dir = String.concat "/" dir_parts in
      FileUtil.mkdir ~parent:true dir;
      let output_file_name =
        Filename.concat dir (List.nth time_zone_parts (len - 1) ^ ".json")
      in
      CCIO.with_out ~flags:output_flags output_file_name (fun oc ->
          Yojson.Basic.pretty_to_channel oc
            (Timedesc_json.Time_zone.to_json tz)))
