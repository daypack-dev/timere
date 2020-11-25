let make_rng ~randomness : unit -> int =
  let randomness = match randomness with [] -> [ 0 ] | _ -> randomness in
  let arr = Array.of_list randomness in
  let len = Array.length arr in
  let cur = ref 0 in
  fun () ->
    let ret = arr.(!cur) in
    cur := (!cur + 1) mod len;
    ret

(* let make ~height ~(randomness : int list) : Time.t =
 *   assert (height > 0);
 *   let open Time in
 *   let rng = make_rng ~randomness in
 *   let rec aux height =
 *     if height = 1 then
 *       match rng () with
 *       | 0 -> Pattern
 *     else
 *   in
 *   aux height *)
