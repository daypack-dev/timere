let db = CCResult.get_exn @@ Timere.Time_zone.Db.Sexp.of_string Time_zone_db.s

let () =
  CCIO.with_out ~flags:[ Open_wronly; Open_creat; Open_trunc ]
    "timere_tzdb.ml"
    (fun oc ->
       Printf.fprintf oc {x|
module M = Map.Make (String)
type entry = {
  is_dst : bool;
  offset : int;
}
type table =
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t *
  entry array
type db = table M.t

let db : db = Marshal.from_string %S 0

let lookup name = M.find_opt name db

let available_time_zones = List.map fst (M.bindings db)
|x}
         (Timere.Time_zone.Db.Raw.dump db)
    )
