exception Modulo_is_invalid

exception Range_is_invalid

type 'a range =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

let map ~(f_inc : 'a * 'a -> 'b * 'b) ~(f_exc : 'a * 'a -> 'b * 'b)
    (t : 'a range) : 'b range =
  match t with
  | `Range_inc (x, y) ->
    let x, y = f_inc (x, y) in
    `Range_inc (x, y)
  | `Range_exc (x, y) ->
    let x, y = f_exc (x, y) in
    `Range_exc (x, y)

let int64_range_of_range (type a) ~(to_int64 : a -> int64) (x : a range) :
  int64 range =
  let f (x, y) = (to_int64 x, to_int64 y) in
  map ~f_inc:f ~f_exc:f x

let int64_inc_range_of_range (type a) ~(to_int64 : a -> int64) (x : a range) :
  int64 * int64 =
  match x with
  | `Range_inc (x, y) -> (to_int64 x, to_int64 y)
  | `Range_exc (x, y) -> (to_int64 x, y |> to_int64 |> Int64.pred)

let int64_exc_range_of_range (type a) ~(to_int64 : a -> int64) (x : a range) :
  int64 * int64 =
  match x with
  | `Range_inc (x, y) -> (to_int64 x, y |> to_int64 |> Int64.succ)
  | `Range_exc (x, y) -> (to_int64 x, to_int64 y)

let inc_range_of_range (type a) ~(to_int64 : a -> int64)
    ~(of_int64 : int64 -> a) (x : a range) : a * a =
  match x with
  | `Range_inc (x, y) -> (x, y)
  | `Range_exc (x, y) -> (x, y |> to_int64 |> Int64.pred |> of_int64)

let exc_range_of_range (type a) ~(to_int64 : a -> int64)
    ~(of_int64 : int64 -> a) (x : a range) : a * a =
  match x with
  | `Range_inc (x, y) -> (x, y |> to_int64 |> Int64.succ |> of_int64)
  | `Range_exc (x, y) -> (x, y)

let join (type a) ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a)
    (x : a range) (y : a range) : a range option =
  let x = int64_exc_range_of_range ~to_int64 x in
  let y = int64_exc_range_of_range ~to_int64 y in
  Time_slot.join x y
  |> Option.map (fun (x, y) -> `Range_exc (of_int64 x, of_int64 y))

let is_valid (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
    (t : a range) : bool =
  match modulo with
  | None ->
    let x, y = int64_exc_range_of_range ~to_int64 t in
    x <= y
  | Some _ -> true

module Flatten = struct
  let flatten_into_seq (type a) ~(modulo : int64 option)
      ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (t : a range) : a Seq.t
    =
    match t with
    | `Range_inc (start, end_inc) -> (
        let start = to_int64 start in
        let end_inc = to_int64 end_inc in
        if start <= end_inc then
          Seq_utils.a_to_b_inc_int64 ~a:start ~b:end_inc |> Seq.map of_int64
        else
          match modulo with
          | None -> raise Range_is_invalid
          | Some modulo ->
            if modulo <= 0L then raise Modulo_is_invalid
            else
              OSeq.append
                (Seq_utils.a_to_b_exc_int64 ~a:start ~b:modulo)
                (Seq_utils.a_to_b_inc_int64 ~a:0L ~b:end_inc)
              |> Seq.map of_int64 )
    | `Range_exc (start, end_exc) -> (
        let start = to_int64 start in
        let end_exc = to_int64 end_exc in
        if start <= end_exc then
          Seq_utils.a_to_b_exc_int64 ~a:start ~b:end_exc |> Seq.map of_int64
        else
          match modulo with
          | None -> raise Range_is_invalid
          | Some modulo ->
            if modulo <= 0L then raise Modulo_is_invalid
            else
              OSeq.append
                (Seq_utils.a_to_b_exc_int64 ~a:start ~b:modulo)
                (Seq_utils.a_to_b_exc_int64 ~a:0L ~b:end_exc)
              |> Seq.map of_int64 )

  let flatten_into_list (type a) ~(modulo : int64 option)
      ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (t : a range) : a list =
    flatten_into_seq ~modulo ~to_int64 ~of_int64 t |> List.of_seq
end

module type B = sig
  type t

  val modulo : int64 option

  val to_int64 : t -> int64

  val of_int64 : int64 -> t
end

module type S = sig
  type t

  val int64_range_of_range : t range -> int64 range

  val int64_inc_range_of_range : t range -> int64 * int64

  val int64_exc_range_of_range : t range -> int64 * int64

  val inc_range_of_range : t range -> t * t

  val exc_range_of_range : t range -> t * t

  val join : t range -> t range -> t range option

  val is_valid : t range -> bool

  module Flatten : sig
    val flatten_into_seq : t range -> t Seq.t

    val flatten_into_list : t range -> t list
  end
end

module Make (B : B) : S with type t := B.t = struct
  open B

  let int64_range_of_range (x : t range) : int64 range =
    int64_range_of_range ~to_int64 x

  let int64_inc_range_of_range (x : t range) : int64 * int64 =
    int64_inc_range_of_range ~to_int64 x

  let int64_exc_range_of_range (x : t range) : int64 * int64 =
    int64_exc_range_of_range ~to_int64 x

  let inc_range_of_range (x : t range) : t * t =
    inc_range_of_range ~to_int64 ~of_int64 x

  let exc_range_of_range (x : t range) : t * t =
    exc_range_of_range ~to_int64 ~of_int64 x

  let join (x : t range) (y : t range) : t range option =
    join ~to_int64 ~of_int64 x y

  let is_valid (x : t range) : bool = is_valid ~modulo ~to_int64 x

  module Flatten = struct
    let flatten_into_seq (t : t range) : t Seq.t =
      Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t

    let flatten_into_list (t : t range) : t list =
      Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t |> List.of_seq
  end
end
