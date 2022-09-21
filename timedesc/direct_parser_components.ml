let uint8 ~pos s : int * int =
  (pos + 1, Char.code (String.get s pos))

let be_uint16 ~pos s : int * int =
  let pos, c0 = uint8 ~pos s in
  let pos, c1 = uint8 ~pos s in
  (pos, c0 * 0x100 + c1)

let take ~pos len s : int * string =
  (pos + len, String.sub s pos len)

