let int_range_of_range (type a) ~(to_int : a -> int) (x : a Range.range) :
  int Range.range =
  let f (x, y) = (to_int x, to_int y) in
  Range.map ~f_inc:f ~f_exc:f x

let int_exc_range_of_range (type a) ~(to_int : a -> int) (x : a Range.range) :
  int * int =
  match x with
  | `Range_inc (x, y) -> (to_int x, y |> to_int |> Int.succ)
  | `Range_exc (x, y) -> (to_int x, to_int y)

let inc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a)
    (x : a Range.range) : a * a =
  match x with
  | `Range_inc (x, y) -> (x, y)
  | `Range_exc (x, y) -> (x, y |> to_int |> Int.pred |> of_int)

let exc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a)
    (x : a Range.range) : a * a =
  match x with
  | `Range_inc (x, y) -> (x, y |> to_int |> Int.succ |> of_int)
  | `Range_exc (x, y) -> (x, y)

let join (type a) ~(to_int : a -> int) ~(of_int : int -> a) (x : a Range.range)
    (y : a Range.range) : a Range.range option =
  let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
  let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
  Range.join ~to_int64 ~of_int64 x y

module Flatten = struct
  let flatten_into_seq (type a) ~(modulo : int option) ~(to_int : a -> int)
      ~(of_int : int -> a) (t : a Range.range) : a Seq.t =
    let modulo = Option.map Int64.of_int modulo in
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    Range.Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t

  let flatten_into_list (type a) ~(modulo : int option) ~(to_int : a -> int)
      ~(of_int : int -> a) (t : a Range.range) : a list =
    flatten_into_seq ~modulo ~to_int ~of_int t |> List.of_seq
end

module type B = sig
  type t

  val modulo : int option

  val to_int : t -> int

  val of_int : int -> t
end

module type S = sig
  type t

  val int_range_of_range : t Range.range -> int Range.range

  val int_exc_range_of_range : t Range.range -> int * int

  val inc_range_of_range : t Range.range -> t * t

  val exc_range_of_range : t Range.range -> t * t

  val join : t Range.range -> t Range.range -> t Range.range option

  module Flatten : sig
    val flatten_into_seq : t Range.range -> t Seq.t

    val flatten_into_list : t Range.range -> t list
  end
end

module Make (B : B) : S with type t := B.t = struct
  open B

  let int_range_of_range (x : t Range.range) : int Range.range =
    int_range_of_range ~to_int x

  let int_exc_range_of_range (x : t Range.range) : int * int =
    int_exc_range_of_range ~to_int x

  let inc_range_of_range (x : t Range.range) : t * t =
    inc_range_of_range ~to_int ~of_int x

  let exc_range_of_range (x : t Range.range) : t * t =
    exc_range_of_range ~to_int ~of_int x

  let join (x : t Range.range) (y : t Range.range) : t Range.range option =
    join ~to_int ~of_int x y

  module Flatten = struct
    let flatten_into_seq (t : t Range.range) : t Seq.t =
      Flatten.flatten_into_seq ~modulo ~to_int ~of_int t

    let flatten_into_list (t : t Range.range) : t list =
      Flatten.flatten_into_seq ~modulo ~to_int ~of_int t |> List.of_seq
  end
end
