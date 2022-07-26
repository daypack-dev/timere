open Sexplib

let sexp_of_int64 x = Sexp.Atom (Int64.to_string x)

let sexp_of_int x = Sexp.Atom (string_of_int x)

let wrap_to_sexp_into_to_sexp_string (f : 'a -> Sexp.t) x =
  Sexp.to_string (f x)
