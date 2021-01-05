type entry = {
  is_dst : bool;
  offset : int;
}

type table = (int64 * entry) array

type record = {
  recorded_offsets : int array;
  table : table;
}

val lookup_record : string -> record option

val available_time_zones : unit -> string list
