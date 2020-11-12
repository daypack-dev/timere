let normalize (type a) ?(skip_filter_invalid = false)
    ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int64 option)
    ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (s : a Range.range Seq.t)
  : a Range.range Seq.t =
  match modulo with
  | None ->
    s
    |> Seq.map (Range.int64_exc_range_of_range ~to_int64)
    |> Time_slots.Normalize.normalize ~skip_filter_invalid ~skip_filter_empty
      ~skip_sort
    |> Seq.map (fun (x, y) -> (of_int64 x, y |> Int64.pred |> of_int64))
    |> Seq.map (fun (x, y) -> `Range_inc (x, y))
  | Some _ ->
    (* not sure what would be a reasonable normalization procedure when domain is a field *)
    s

module Check = struct
  let seq_is_valid (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
      (s : a Range.range Seq.t) : bool =
    OSeq.for_all (Range.is_valid ~modulo ~to_int64) s

  let list_is_valid (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
      (s : a Range.range list) : bool =
    List.for_all (Range.is_valid ~modulo ~to_int64) s
end

module Flatten = struct
  let flatten (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
      ~(of_int64 : int64 -> a) (s : a Range.range Seq.t) : a Seq.t =
    Seq.flat_map (Range.Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64) s

  let flatten_list (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64)
      ~(of_int64 : int64 -> a) (l : a Range.range list) : a list =
    l |> List.to_seq |> flatten ~modulo ~to_int64 ~of_int64 |> List.of_seq
end

module Of_seq = struct
  let range_seq_of_seq (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int64 option)
      ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (s : a Seq.t) :
    a Range.range Seq.t =
    s
    |> Seq.map (fun x -> `Range_inc (x, x))
    |> normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
      ~to_int64 ~of_int64

  let range_list_of_seq (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int64 option)
      ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (s : a Seq.t) :
    a Range.range list =
    range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
      ~to_int64 ~of_int64 s
    |> List.of_seq
end

module Of_list = struct
  let range_seq_of_list (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int64 option)
      ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (l : a list) :
    a Range.range Seq.t =
    List.to_seq l
    |> Of_seq.range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty
      ~skip_sort ~modulo ~to_int64 ~of_int64

  let range_list_of_list (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int64 option)
      ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (l : a list) :
    a Range.range list =
    List.to_seq l
    |> Of_seq.range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty
      ~skip_sort ~modulo ~to_int64 ~of_int64
    |> List.of_seq
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

module Make (B : Range.B) : S with type t := B.t = struct
  open B

  let normalize ?(skip_filter_invalid = false) ?(skip_filter_empty = false)
      ?(skip_sort = false) (s : t Range.range Seq.t) =
    normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
      ~to_int64 ~of_int64 s

  module Check = struct
    let seq_is_valid s = Check.seq_is_valid ~modulo ~to_int64 s

    let list_is_valid l = Check.list_is_valid ~modulo ~to_int64 l
  end

  module Flatten = struct
    let flatten (s : t Range.range Seq.t) : t Seq.t =
      Flatten.flatten ~modulo ~to_int64 ~of_int64 s

    let flatten_list (l : t Range.range list) : t list =
      Flatten.flatten_list ~modulo ~to_int64 ~of_int64 l
  end

  module Of_seq = struct
    let range_seq_of_seq (s : t Seq.t) : t Range.range Seq.t =
      Of_seq.range_seq_of_seq ~modulo ~to_int64 ~of_int64 s

    let range_list_of_seq (s : t Seq.t) : t Range.range list =
      Of_seq.range_list_of_seq ~modulo ~to_int64 ~of_int64 s
  end

  module Of_list = struct
    let range_seq_of_list (l : t list) : t Range.range Seq.t =
      List.to_seq l |> Of_seq.range_seq_of_seq

    let range_list_of_list (l : t list) : t Range.range list =
      List.to_seq l |> Of_seq.range_seq_of_seq |> List.of_seq
  end
end
