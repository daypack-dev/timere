val normalize :
  ?skip_filter_invalid:bool ->
  ?skip_filter_empty:bool ->
  ?skip_sort:bool ->
  modulo:int option ->
  to_int:('a -> int) ->
  of_int:(int -> 'a) ->
  'a Range.range Seq.t ->
  'a Range.range Seq.t

module Flatten : sig
  val flatten :
    modulo:int option ->
    to_int:('a -> int) ->
    of_int:(int -> 'a) ->
    'a Range.range Seq.t ->
    'a Seq.t

  val flatten_list :
    modulo:int option ->
    to_int:('a -> int) ->
    of_int:(int -> 'a) ->
    'a Range.range list ->
    'a list
end

module Of_seq : sig
  val range_seq_of_seq :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    modulo:int option ->
    to_int:('a -> int) ->
    of_int:(int -> 'a) ->
    'a Seq.t ->
    'a Range.range Seq.t

  val range_list_of_seq :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    modulo:int option ->
    to_int:('a -> int) ->
    of_int:(int -> 'a) ->
    'a Seq.t ->
    'a Range.range list
end

module Of_list : sig
  val range_seq_of_list :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    modulo:int option ->
    to_int:('a -> int) ->
    of_int:(int -> 'a) ->
    'a list ->
    'a Range.range Seq.t

  val range_list_of_list :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    modulo:int option ->
    to_int:('a -> int) ->
    of_int:(int -> 'a) ->
    'a list ->
    'a Range.range list
end

module Make (B : Range_small.B) : Ranges.S with type t := B.t
