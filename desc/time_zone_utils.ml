let get_local_tz_for_arg () =
  CCOpt.get_exn_or
    "get_local_tz_for_arg: Could not determine the local timezone. Please \
     specify ~tz* explicitly or use an appropriate timere.tzlocal.* module."
    (Time_zone.local ())
