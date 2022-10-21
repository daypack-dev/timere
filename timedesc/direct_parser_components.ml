let uint8 ~pos s : int * int =
  (pos + 1, Char.code (String.get s pos))

let be_uint16 ~pos s : int * int =
  let pos, c0 = uint8 ~pos s in
  let pos, c1 = uint8 ~pos s in
  (pos, c0 * 0x100 + c1)

let be_int32 ~pos s : int * Int32.t =
  let pos, c01 = be_uint16 ~pos s in
  let pos, c23 = be_uint16 ~pos s in
  (pos, Int32.(add
                 (shift_left (of_int c01) 16)
                 (of_int c23)))

let be_int64 ~pos s : int * Int64.t =
  let pos, c0123 = be_int32 ~pos s in
  let pos, c4567 = be_int32 ~pos s in
  (pos, Int64.(add
                 (shift_left (of_int32 c0123) 32)
                 (of_int32 c4567)))

let take ~pos len s : int * string =
  (pos + len, String.sub s pos len)

let count (type a) n (f : pos:int -> string -> int * a) ~pos s : int * a list =
  let buf = ref [] in
  let pos = ref pos in
  assert (n >= 0);
  for i=0 to n-1 do
    let pos, x = f ~pos:!pos s in
    buf := x :: !buf;
  done;
  (!pos, List.rev !buf)
