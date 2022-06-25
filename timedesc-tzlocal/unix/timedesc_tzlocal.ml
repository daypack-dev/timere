let read_lines_l (ic : in_channel) : string list =
  let l = ref [] in
  try
    while true do
      l := input_line ic :: !l
    done;
    failwith "Unreachable"
  with
  | End_of_file -> List.rev !l

let with_in (file : string) (f : in_channel -> 'a) : 'a =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> f ic)

let local () : string list =
  (* Approach copied from Python package tzlocal:
     https://github.com/regebro/tzlocal
  *)
  match Sys.getenv_opt "TZ" with
  | Some name -> [ name ]
  | None -> (
      if Sys.file_exists "/system/bin/getprop" then (
        (* if we are under Termux on Android *)
        let ic = Unix.open_process_in "getprop" in
        let name = read_lines_l ic in
        close_in ic;
        name)
      else
        (* Look for distribution specific configs *)
        let try1 =
          List.fold_left
            (fun tz file ->
               match tz with
               | Some tz -> Some tz
               | None -> (
                   try
                     with_in file (fun ic ->
                         let lines = read_lines_l ic in
                         match lines with
                         | [] -> None
                         | x :: _ ->
                           if String.sub x 0 5 = "TZif2" then None
                           else
                             List.fold_left
                               (fun tz line ->
                                  match tz with
                                  | Some tz -> Some tz
                                  | None ->
                                    let name =
                                      line
                                      |> String.split_on_char ' '
                                      |> List.hd
                                      |> String.split_on_char '#'
                                      |> List.hd
                                    in
                                    Some name)
                               None lines)
                   with _ -> None))
            None
            [ "/etc/timezone"; "/var/db/zoneinfo" ]
        in
        match try1 with
        | Some name -> [ name ]
        | None -> (
            (* CentOS has a ZONE setting in /etc/sysconfig/clock,
               OpenSUSE has a TIMEZONE setting in /etc/sysconfig/clock and
               Gentoo has a TIMEZONE setting in /etc/conf.d/clock
               We look through these files for a timezone
            *)
            let try2 =
              List.fold_left
                (fun tz file ->
                   match tz with
                   | Some tz -> Some tz
                   | None -> (
                       try
                         with_in file (fun ic ->
                             let lines = read_lines_l ic in
                             List.fold_left
                               (fun tz line ->
                                  match tz with
                                  | Some tz -> Some tz
                                  | None -> (
                                      let name =
                                        try
                                          Some
                                            (Scanf.sscanf line {| ZONE = "%[^"]"|}
                                               Fun.id)
                                        with _ -> (
                                            try
                                              Some
                                                (Scanf.sscanf line
                                                   {| TIMEZONE = "%[^"]"|} Fun.id)
                                            with _ -> None)
                                      in
                                      match name with
                                      | None -> None
                                      | Some s ->
                                        Some
                                          (s
                                           |> String.split_on_char ' '
                                           |> String.concat "_")))
                               None lines)
                       with _ -> None))
                None
                [ "/etc/sysconfig/clock"; "/etc/conf.d/clock" ]
            in
            match try2 with
            | Some name -> [ name ]
            | None ->
              (* systemd distributions use symlinks that include the zone name *)
              let try3 =
                try
                  let file = "/etc/localtime" in
                  let real_path = Unix.readlink file in
                  let parts = String.split_on_char '/' real_path in
                  let combinations, _ =
                    List.fold_left
                      (fun (acc, parts) _ -> (parts :: acc, List.tl parts))
                      ([], parts) parts
                  in
                  List.map (String.concat "/") combinations
                with
                | _ -> []
              in
              try3))
