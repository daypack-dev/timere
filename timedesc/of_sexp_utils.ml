open Sexplib

exception Invalid_data of string

let invalid_data s = raise (Invalid_data s)

let int_of_sexp (x : Sexp.t) =
  match x with
  | Atom s -> (
      try int_of_string s
      with Failure _ ->
        invalid_data (Printf.sprintf "Failed to parse int: %s" s))
  | List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for int: %s" (Sexplib.Sexp.to_string x))

let int64_of_sexp (x : Sexp.t) =
  match x with
  | Atom s -> (
      try Int64.of_string s
      with Failure _ ->
        invalid_data (Printf.sprintf "Failed to parse int64: %s" s))
  | List _ ->
    invalid_data
      (Printf.sprintf "Expected atom for int64: %s" (Sexplib.Sexp.to_string x))

let wrap_of_sexp (f : Sexp.t -> 'a) : Sexp.t -> ('a, string) result =
  fun x ->
  try Ok (f x) with
  | Invalid_data msg -> Error msg
  | Invalid_argument msg -> Error msg

let wrap_of_sexp_into_of_sexp_string (f : Sexplib.Sexp.t -> 'a) :
  string -> ('a, string) result =
  fun s ->
  match Sexplib.Sexp.of_string s with
  | exception _ -> Error "Failed to parse string into sexp"
  | x -> (wrap_of_sexp f) x
