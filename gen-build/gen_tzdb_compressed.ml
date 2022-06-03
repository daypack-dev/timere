let file_input = Sys.argv.(1)

let file_output = Sys.argv.(2)

let () =
  Format.printf "Generating file %s from file %s@." file_output file_input;
  let ic =
    open_in file_input
  in
  let db =
    Fun.protect ~finally:(fun () -> close_in ic)
      (fun () ->
         really_input_string ic (in_channel_length ic)
         |> Timedesc.Time_zone.Db.Sexp.of_string
         |> Option.get
      )
  in
  let oc =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o766 file_output
  in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () ->
       Printf.fprintf oc "let s = %S" (Timedesc.Time_zone.Db.Compressed.dump db))
