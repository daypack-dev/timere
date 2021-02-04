let local () : string list =
  (* Approached copied from Python package tzlocal:
     https://github.com/regebro/tzlocal
  *)
  match Sys.getenv_opt "TZ" with
  | Some name -> [name]
  | None ->
    if CCIO.File.exists "/system/bin/getprop" then
      (* if we are under Termux on Android *)
      let ic = Unix.open_process_in "getprop" in
      let name =
          (CCIO.read_all ic) in
      close_in ic;
      [name]
    else
      (* Look for distribution specific configs *)
      let try1 =
        List.fold_left (fun tz file ->
            match tz with
            | Some tz -> Some tz
            | None ->
              try
              CCIO.with_in ~flags:[ Open_rdonly; ] file (fun ic ->
                  let lines =
                    CCIO.read_lines_l ic in
                  match lines with
                  | [] -> None
                  | x :: _ ->
                    if CCString.sub x 0 5 = "TZif2" then
                      None
                    else
                      List.fold_left (fun tz line ->
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
                            Some name
                        )
                        None
                        lines
                )
              with
              | _ -> None
          )
          None
          [ "/etc/timezone"; "/var/db/zoneinfo"]
      in
      match try1 with
      | Some name -> [name]
      | None ->
        (* CentOS has a ZONE setting in /etc/sysconfig/clock,
           OpenSUSE has a TIMEZONE setting in /etc/sysconfig/clock and
           Gentoo has a TIMEZONE setting in /etc/conf.d/clock
           We look through these files for a timezone
        *)
        let try2 =
        List.fold_left (fun tz file ->
            match tz with
            | Some tz -> Some tz
            | None ->
              try
              CCIO.with_in ~flags:[Open_rdonly; ] file (fun ic ->
                  let lines =
                    CCIO.read_lines_l ic in
                  List.fold_left (fun tz line ->
                      match tz with
                      | Some tz -> Some tz
                      | None ->
                        let name =
                          try
                            Some (
                              Scanf.sscanf line {| ZONE = "%[^"]"|} CCFun.id
                            )
                          with
                          | _ ->
                            try
                              Some (
                                Scanf.sscanf line {| TIMEZONE = "%[^"]"|} CCFun.id
                              )
                            with
                            | _ -> None
                        in
                        match name with
                        | None -> None
                        | Some s ->
                          Some (CCString.replace ~sub:" " ~by:"_" s)
                    )
                    None
                    lines
                  )
              with
              | _ -> None
          )
        None
        ["/etc/sysconfig/clock"; "/etc/conf.d/clock"]
        in
        match try2 with
        | Some name -> [name]
        | None ->
          (* systemd distributions use symlinks that include the zone name *)
          let try3 =
            let file = "/etc/localtime" in
            if FileUtil.(test Is_link file) then
              let real_path = FileUtil.readlink file in
              let parts = String.split_on_char '/' real_path in
              let combinations, _ =
                List.fold_left (fun (acc, parts) _ ->
                    (parts :: acc, List.tl parts)
                  )
                  ([], parts)
                  parts
              in
              List.map (String.concat "/") combinations
            else
              []
          in
          try3
