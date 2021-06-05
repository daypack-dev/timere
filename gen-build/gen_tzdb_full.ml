let file_input = Sys.argv.(1)

let file_output = Sys.argv.(2)

let () =
  Format.printf "Generating file %s from file %s@." file_output file_input;
  let db =
    CCOpt.get_exn_or "Expected db to successfully load from sexp string"
    @@ Timedesc.Time_zone.Db.Sexp.of_string
    @@ CCIO.(with_in file_input read_all)
  in
  CCIO.with_out ~flags:[ Open_wronly; Open_creat; Open_trunc ] file_output
    (fun oc ->
      Printf.fprintf oc "let s = %S" (Timedesc.Time_zone.Db.Raw.dump db))
