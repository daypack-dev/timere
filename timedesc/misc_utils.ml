let int32_int32_of_int64 (x : int64) : int32 * int32 =
  (Int64.shift_right_logical x 32 |> Int64.to_int32, x |> Int64.to_int32)

let int64_of_int32_int32 ((x, y) : int32 * int32) : int64 =
  let left = Int64.shift_left (Int64.of_int32 x) 32 in
  let right = Int64.logand 0x00000000FFFFFFFFL (Int64.of_int32 y) in
  Int64.logor left right

let take_first_n_list (n : int) (l : 'a list) : 'a list =
  let rec aux n acc l =
    if n = 0 then List.rev acc
    else
      match l with [] -> aux 0 acc [] | x :: xs -> aux (pred n) (x :: acc) xs
  in
  assert (n >= 0);
  aux n [] l

let convert_of_int_to_int64 (f : int -> 'a) : int64 -> 'a =
  fun x -> x |> Int64.to_int |> f

let convert_to_int_to_int64 (f : 'a -> int) : 'a -> int64 =
  fun x -> x |> f |> Int64.of_int

let get_ok_error_list (l : ('a, 'b) result list) : ('a list, 'b) result =
  List.find_opt Result.is_error l
  |> (fun x ->
      match x with
      | None -> None
      | Some (Ok _) -> None
      | Some (Error x) -> Some x)
  |> fun x ->
  match x with None -> Ok (List.map Result.get_ok l) | Some x -> Error x

let list_concat_map (f : 'a -> 'b list) (l : 'a list) : 'b list =
  List.to_seq l
  |> Seq.flat_map (fun x -> f x |> List.to_seq)
  |> List.of_seq

let list_concat_mapi (f : int -> 'a -> 'b list) (l : 'a list) : 'b list =
  List.to_seq l
  |> Seq_utils_.mapi (fun i x -> (i, x))
  |> Seq.flat_map (fun (i, x) -> f i x |> List.to_seq)
  |> List.of_seq

let last_element_of_list (l : 'a list) : 'a option =
  let rec aux l =
    match l with [] -> None | [ x ] -> Some x | _ :: rest -> aux rest
  in
  aux l

let option_get_exn_or (s : string) (x : 'a option) : 'a =
  match x with
  | None -> invalid_arg s
  | Some x -> x

let array_find_idx (f : 'a -> bool) (arr : 'a array) : (int * 'a) option =
  let rec aux f i arr =
    if i < Array.length arr then (
      if f arr.(i) then
        Some (i, arr.(i))
      else
        aux f (succ i) arr
    ) else
      None
  in
  aux f 0 arr
