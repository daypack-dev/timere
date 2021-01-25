exception Invalid_data of string

let invalid_data s = raise (Invalid_data s)

let int_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      try int_of_string s
      with Failure _ ->
        invalid_data (Printf.sprintf "Failed to parse int: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for int: %s" (CCSexp.to_string x))

let int64_of_sexp (x : CCSexp.t) =
  match x with
  | `Atom s -> (
      try Int64.of_string s
      with Failure _ ->
        invalid_data (Printf.sprintf "Failed to parse int64: %s" s))
  | `List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for int64: %s" (CCSexp.to_string x))
