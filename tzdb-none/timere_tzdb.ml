type entry = {
  is_dst : bool;
  offset : int;
}

type table = (int64 * entry) array

let lookup _ = None

let available_time_zones = []
