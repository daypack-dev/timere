let get_local_tz_for_arg () =
  let tz = Time_zone.local () in
  match tz with
  | Some tz -> tz
  | None ->
    invalid_arg
      "get_local_tz_for_arg: Could not determine the local timezone. Please \
       specify ~tz* explicitly or use an appropriate timere.tzlocal.* \
       module."
