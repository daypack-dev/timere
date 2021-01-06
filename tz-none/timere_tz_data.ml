exception Tz_data_retrieval_failure of string

type entry = {
  is_dst : bool;
  offset : int;
}

type table = (int64 * entry) array

type record = {
  recorded_offsets : int array;
  table : table;
}

let lookup_record _ = None

let available_time_zones () = []
