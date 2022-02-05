module Int64_range = Time.Range.Make (struct
    type t = int64

    let modulo = None

    let to_int64 x = x

    let of_int64 x = x
  end)

module Int_range = Time.Range_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Alco = struct
  let int64_range_flatten1 () =
    Alcotest.(check (list int64))
      "same list"
      [ 1L; 2L; 3L; 4L; 5L; 6L; 7L ]
      (Int64_range.Flatten.flatten_into_list (`Range_exc (1L, 8L)))

  let int64_range_flatten2 () =
    Alcotest.(check (list int64))
      "same list"
      [ 1L; 2L; 3L; 4L; 5L; 6L; 7L ]
      (Int64_range.Flatten.flatten_into_list (`Range_inc (1L, 7L)))

  let suite =
    [
      Alcotest.test_case "int64_range_flatten1" `Quick int64_range_flatten1;
      Alcotest.test_case "int64_range_flatten2" `Quick int64_range_flatten2;
    ]
end
