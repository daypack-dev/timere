open Fuzz_utils

let () =
  Crowbar.add_test ~name:"to_of_sexp" [ time ] (fun t ->
      let t' = t |> To_sexp.to_sexp |> Of_sexp.of_sexp |> Result.get_ok in
      Crowbar.check_eq ~eq:Time.equal t t')
