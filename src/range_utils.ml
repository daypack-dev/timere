let result_range_get (x : ('a, 'b) result Range.range) : 'a Range.range option =
  match x with
  | `Range_inc (x, y) -> (
      match (x, y) with Ok x, Ok y -> Some (`Range_inc (x, y)) | _, _ -> None )
  | `Range_exc (x, y) -> (
      match (x, y) with Ok x, Ok y -> Some (`Range_exc (x, y)) | _, _ -> None )
