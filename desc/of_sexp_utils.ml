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

let wrap_of_sexp (f : CCSexp.t -> 'a) : CCSexp.t -> ('a, string) result =
 fun x ->
  try Ok (f x) with
  | Invalid_data msg -> Error msg
  | Invalid_argument msg -> Error msg

let wrap_of_sexp_into_of_sexp_string (f : CCSexp.t -> 'a) :
    string -> ('a, string) result =
 fun s ->
  let res =
    try CCSexp.parse_string s
    with _ -> Error "Failed to parse string into sexp"
  in
  match res with Error msg -> Error msg | Ok x -> (wrap_of_sexp f) x
