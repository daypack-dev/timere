let get_local_tz_for_arg () =
  match Time_zone.local () with
  | None -> invalid_arg
              "get_local_tz_for_arg: Could not determine the local timezone. Please \
               specify ~tz* explicitly or use an appropriate timedesc-tzlocal.* module."
  | Some x -> x
