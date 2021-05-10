let () =
  let open Timere in
  match
    resolve (
      after (Timedesc.now ())
      & months [12]
      & days [25]
      & weekdays [`Wed]
    )
  with
  | Error msg -> failwith msg
  | Ok s ->
    Fmt.pr "%a@." (Timedesc.Interval.pp_seq ~sep:(Fmt.any "@.") ()) s
