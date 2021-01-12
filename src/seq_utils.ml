open Int64_utils

let nat : int Seq.t =
  let open OSeq.Generator in
  let rec aux n = yield n >>= fun () -> aux (n + 1) in
  run (aux 0)

let zero_to_n_exc n : int Seq.t =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur + 1) n) else Seq.empty
  in
  aux 0 n

let zero_to_n_inc n = zero_to_n_exc (n + 1)

let a_to_b_exc_int64 ~a ~b : int64 Seq.t =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur +^ 1L) n) else Seq.empty
  in
  aux a b

let a_to_b_inc_int64 ~a ~b : int64 Seq.t = a_to_b_exc_int64 ~a ~b:(b +^ 1L)

let zero_to_n_exc_int64 n : int64 Seq.t = a_to_b_exc_int64 ~a:0L ~b:n

let zero_to_n_inc_int64 n = zero_to_n_exc_int64 (n +^ 1L)

let mod_int n =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur + 1) n) else aux 0 n
  in
  aux 0 n

let collect_round_robin (type a) ~(f_le : a -> a -> bool)
    (batches : a Seq.t list) : a option list Seq.t =
  let rec get_usable_part (cur : a) (seq : a Seq.t) : a Seq.t =
    match seq () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (x, rest) as s ->
      if f_le cur x then fun () -> s else get_usable_part cur rest
  in
  let rec aux (cur : a option) (batches : a Seq.t list) : a option list Seq.t =
    let cur, acc, new_batches =
      List.fold_left
        (fun (cur, acc, new_batches) seq ->
           let usable =
             match cur with
             | None -> seq
             | Some cur_start -> get_usable_part cur_start seq
           in
           match usable () with
           | Seq.Nil -> (cur, None :: acc, new_batches)
           | Seq.Cons (x, rest) -> (Some x, Some x :: acc, rest :: new_batches))
        (cur, [], []) batches
    in
    if List.exists CCOpt.is_some acc then
      let acc = List.rev acc in
      let new_batches = List.rev new_batches in
      fun () -> Seq.Cons (acc, aux cur new_batches)
    else Seq.empty
  in
  aux None batches

let check_if_f_holds_for_immediate_neighbors (type a) ~(f : a -> a -> bool)
    ~(f_exn : a -> a -> exn) (s : a Seq.t) : a Seq.t =
  let rec aux f f_exn (cur : a option) (s : a Seq.t) : a Seq.t =
    match s () with
    | Seq.Nil -> ( match cur with None -> Seq.empty | Some x -> Seq.return x)
    | Seq.Cons (x, rest) -> (
        match cur with
        | None -> aux f f_exn (Some x) rest
        | Some cur ->
          if f cur x then fun () -> Seq.Cons (cur, aux f f_exn (Some x) rest)
          else raise (f_exn cur x))
  in
  aux f f_exn None s

let get_ok_error_list (s : ('a, 'b) result Seq.t) : ('a list, 'b) result =
  let rec aux acc (s : ('a, 'b) result Seq.t) : ('a list, 'b) result =
    match s () with
    | Seq.Nil -> Ok (List.rev acc)
    | Seq.Cons (x, rest) -> (
        match x with Ok x -> aux (x :: acc) rest | Error s -> Error s)
  in
  aux [] s

let last_element_of_seq (s : 'a Seq.t) : 'a option =
  let rec aux s =
    match s () with
    | Seq.Nil -> None
    | Seq.Cons (x, rest) -> (
        match rest () with Seq.Nil -> Some x | Seq.Cons _ -> aux rest)
  in
  aux s
