let zero_to_n_exc n : int Seq.t =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur + 1) n) else Seq.empty
  in
  aux 0 n

let zero_to_n_inc n = zero_to_n_exc (n + 1)

let mapi (f : int -> 'a -> 'b) (s : 'a Seq.t) : 'b Seq.t =
  let rec aux f cur s =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (x, rest) ->
      fun () ->
        Seq.Cons (f cur x, aux f (succ cur) rest)
  in
  aux f 0 s

(* Copied from Fmt *)

let iter ?sep:(pp_sep = Format.pp_print_cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_elt ppf v
  in
  iter pp_elt v

let pp ?sep pp_elt = iter ?sep Seq.iter pp_elt
