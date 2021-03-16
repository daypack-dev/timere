open Fuzz_utils

let () =
  Crowbar.add_test ~name:"to_of_sexp" [ time ] (fun t ->
      let t' = t |> To_sexp.to_sexp |> Of_sexp.of_sexp in
      Crowbar.check_eq ~pp:Printers.pp_sexp ~eq:Time.equal t t')
