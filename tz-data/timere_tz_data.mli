type entry = {
  is_dst : bool;
  offset : int;
}

type table = (int64 * entry) array

type record = {
  recorded_offsets : int array;
  table : table;
}

module String_map : Map.S with type key = string

type db = record String_map.t

val db : db
