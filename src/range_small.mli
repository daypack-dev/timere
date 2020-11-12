val int_range_of_range : to_int:('a -> int) -> 'a Range.range -> int Range.range

val int_exc_range_of_range : to_int:('a -> int) -> 'a Range.range -> int * int

val inc_range_of_range :
  to_int:('a -> int) -> of_int:(int -> 'a) -> 'a Range.range -> 'a * 'a

val exc_range_of_range :
  to_int:('a -> int) -> of_int:(int -> 'a) -> 'a Range.range -> 'a * 'a

val join :
  to_int:('a -> int) ->
  of_int:(int -> 'a) ->
  'a Range.range ->
  'a Range.range ->
  'a Range.range option

module Flatten : sig
  val flatten_into_seq :
    modulo:int option ->
    to_int:('a -> int) ->
    of_int:(int -> 'a) ->
    'a Range.range ->
    'a Seq.t

  val flatten_into_list :
    modulo:int option ->
    to_int:('a -> int) ->
    of_int:(int -> 'a) ->
    'a Range.range ->
    'a list
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

module Make (B : B) : S with type t := B.t
