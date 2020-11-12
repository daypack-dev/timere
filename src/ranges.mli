val normalize :
  ?skip_filter_invalid:bool ->
  ?skip_filter_empty:bool ->
  ?skip_sort:bool ->
  modulo:int64 option ->
  to_int64:('a -> int64) ->
  of_int64:(int64 -> 'a) ->
  'a Range.range Seq.t ->
  'a Range.range Seq.t

module Check : sig
  val seq_is_valid :
    modulo:int64 option ->
    to_int64:('a -> int64) ->
    'a Range.range Seq.t ->
    bool

  val list_is_valid :
    modulo:int64 option -> to_int64:('a -> int64) -> 'a Range.range list -> bool
end

module Flatten : sig
  val flatten :
    modulo:int64 option ->
    to_int64:('a -> int64) ->
    of_int64:(int64 -> 'a) ->
    'a Range.range Seq.t ->
    'a Seq.t

  val flatten_list :
    modulo:int64 option ->
    to_int64:('a -> int64) ->
    of_int64:(int64 -> 'a) ->
    'a Range.range list ->
    'a list
end

module Of_seq : sig
  val range_seq_of_seq :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    modulo:int64 option ->
    to_int64:('a -> int64) ->
    of_int64:(int64 -> 'a) ->
    'a Seq.t ->
    'a Range.range Seq.t

  val range_list_of_seq :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    modulo:int64 option ->
    to_int64:('a -> int64) ->
    of_int64:(int64 -> 'a) ->
    'a Seq.t ->
    'a Range.range list
end

module Of_list : sig
  val range_seq_of_list :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    modulo:int64 option ->
    to_int64:('a -> int64) ->
    of_int64:(int64 -> 'a) ->
    'a list ->
    'a Range.range Seq.t

  val range_list_of_list :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    modulo:int64 option ->
    to_int64:('a -> int64) ->
    of_int64:(int64 -> 'a) ->
    'a list ->
    'a Range.range list
end

module type S = sig
  type t

  val normalize :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    t Range.range Seq.t ->
    t Range.range Seq.t

  module Check : sig
    val seq_is_valid : t Range.range Seq.t -> bool

    val list_is_valid : t Range.range list -> bool
  end

  module Flatten : sig
    val flatten : t Range.range Seq.t -> t Seq.t

    val flatten_list : t Range.range list -> t list
  end

  module Of_seq : sig
    val range_seq_of_seq : t Seq.t -> t Range.range Seq.t

    val range_list_of_seq : t Seq.t -> t Range.range list
  end

  module Of_list : sig
    val range_seq_of_list : t list -> t Range.range Seq.t

    val range_list_of_list : t list -> t Range.range list
  end
end

module Make (B : Range.B) : S with type t := B.t
