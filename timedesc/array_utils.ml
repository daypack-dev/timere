let for_all2 (f : 'a -> 'b -> bool) (arr1 : 'a array) (arr2 : 'b array) : bool =
  assert (Array.length arr1 = Array.length arr2);
  let rec aux cur len arr1 arr2 =
    if cur < len then
      f arr1.(cur) arr2.(cur)
      && aux (cur + 1) len arr1 arr2
    else
      true
  in
  aux 0 (Array.length arr1) arr1 arr2
