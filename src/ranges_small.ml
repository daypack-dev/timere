let normalize (type a) ?(skip_filter_invalid = false)
    ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
    ~(to_int : a -> int) ~(of_int : int -> a) (s : a Range.range Seq.t) :
  a Range.range Seq.t =
  let modulo = Option.map Int64.of_int modulo in
  let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
  let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
  Ranges.normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
    ~to_int64 ~of_int64 s

module Check = struct
  let seq_is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
      (s : a Range.range Seq.t) : bool =
    let modulo = Option.map Int64.of_int modulo in
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    Ranges.Check.seq_is_valid ~modulo ~to_int64 s

  let list_is_valid (type a) ~(modulo : int option) ~(to_int : a -> int)
      (l : a Range.range list) : bool =
    let modulo = Option.map Int64.of_int modulo in
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    Ranges.Check.list_is_valid ~modulo ~to_int64 l
end

module Flatten = struct
  let flatten (type a) ~(modulo : int option) ~(to_int : a -> int)
      ~(of_int : int -> a) (s : a Range.range Seq.t) : a Seq.t =
    let modulo = Option.map Int64.of_int modulo in
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    Ranges.Flatten.flatten ~modulo ~to_int64 ~of_int64 s

  let flatten_list (type a) ~(modulo : int option) ~(to_int : a -> int)
      ~(of_int : int -> a) (l : a Range.range list) : a list =
    l |> List.to_seq |> flatten ~modulo ~to_int ~of_int |> List.of_seq
end

module Of_seq = struct
  let range_seq_of_seq (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
      ~(to_int : a -> int) ~(of_int : int -> a) (s : a Seq.t) :
    a Range.range Seq.t =
    s
    |> Seq.map (fun x -> `Range_inc (x, x))
    |> normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
      ~to_int ~of_int

  let range_list_of_seq (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
      ~(to_int : a -> int) ~(of_int : int -> a) (s : a Seq.t) :
    a Range.range list =
    range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo
      ~to_int ~of_int s
    |> List.of_seq
end

module Of_list = struct
  let range_seq_of_list (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
      ~(to_int : a -> int) ~(of_int : int -> a) (l : a list) :
    a Range.range Seq.t =
    List.to_seq l
    |> Of_seq.range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty
      ~skip_sort ~modulo ~to_int ~of_int

  let range_list_of_list (type a) ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) ~(modulo : int option)
      ~(to_int : a -> int) ~(of_int : int -> a) (l : a list) :
    a Range.range list =
    List.to_seq l
    |> Of_seq.range_seq_of_seq ~skip_filter_invalid ~skip_filter_empty
      ~skip_sort ~modulo ~to_int ~of_int
    |> List.of_seq
end

module Make (B : Range_small.B) : Ranges.S with type t := B.t = struct
  open B

  let normalize ?(skip_filter_invalid = false) ?(skip_filter_empty = false)
      ?(skip_sort = false) (s : t Range.range Seq.t) =
    normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort ~modulo ~to_int
      ~of_int s

  module Check = struct
    let seq_is_valid s = Check.seq_is_valid ~modulo ~to_int s

    let list_is_valid l = Check.list_is_valid ~modulo ~to_int l
  end

  module Flatten = struct
    let flatten (s : t Range.range Seq.t) : t Seq.t =
      Flatten.flatten ~modulo ~to_int ~of_int s

    let flatten_list (l : t Range.range list) : t list =
      Flatten.flatten_list ~modulo ~to_int ~of_int l
  end

  module Of_seq = struct
    let range_seq_of_seq (s : t Seq.t) : t Range.range Seq.t =
      Of_seq.range_seq_of_seq ~modulo ~to_int ~of_int s

    let range_list_of_seq (s : t Seq.t) : t Range.range list =
      Of_seq.range_list_of_seq ~modulo ~to_int ~of_int s
  end

  module Of_list = struct
    let range_seq_of_list (l : t list) : t Range.range Seq.t =
      List.to_seq l |> Of_seq.range_seq_of_seq

    let range_list_of_list (l : t list) : t Range.range list =
      List.to_seq l |> Of_seq.range_seq_of_seq |> List.of_seq
  end
end
