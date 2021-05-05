let sexp_of_int64 x = CCSexp.atom @@ Int64.to_string x

let sexp_of_int x = CCSexp.atom @@ string_of_int x

let wrap_to_sexp_into_to_sexp_string (f : 'a -> CCSexp.t) x =
  CCSexp.to_string (f x)
