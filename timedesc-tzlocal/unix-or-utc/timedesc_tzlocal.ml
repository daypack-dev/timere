let local () =
  match Unix_timedesc_tzlocal.local () with
  | [] -> [ "UTC" ]
  | l -> l
