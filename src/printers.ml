let pad_int (c : char option) (x : int) : string =
  match c with
  | None -> string_of_int x
  | Some c -> if x < 10 then Printf.sprintf "%c%d" c x else string_of_int x

let pp_hms formatter (hms : Time.Hms'.t) : unit =
  Fmt.pf formatter "%d:%d:%d" hms.hour hms.minute hms.second

let string_of_hms hms = Fmt.str "%a" pp_hms hms

let wrap_to_sexp_into_pp_sexp (f : 'a -> CCSexp.t) :
  Format.formatter -> 'a -> unit =
  fun formatter x -> CCSexp.pp formatter (f x)

let pp_sexp = wrap_to_sexp_into_pp_sexp To_sexp.to_sexp
