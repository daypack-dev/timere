module M : Map.S with type key = string

type entry = {
  is_dst : bool;
  offset : int;
}

type table =
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * entry array

val db : table M.t
