let wrap_to_sexp_into_pp_sexp (f : 'a -> CCSexp.t) :
  Format.formatter -> 'a -> unit =
  fun formatter x -> CCSexp.pp formatter (f x)
