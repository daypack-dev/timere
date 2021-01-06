type entry = {
  is_dst : bool;
  offset : int;
}

type table = (int64 * entry) array

val lookup : string -> table option

val available_time_zones : string list
