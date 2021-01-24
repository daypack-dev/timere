type entry = {
  is_dst : bool;
  offset : int;
}

type table =
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t *
  entry array

val lookup : string -> table option

val available_time_zones : string list
