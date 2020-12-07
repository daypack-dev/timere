let () =
  Crowbar.add_test ~name:"of_sexp_string_does_not_crash" [ Crowbar.bytes ]
    (fun s -> Of_sexp.of_sexp_string s |> ignore)
