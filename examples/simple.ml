let () =
  let open Timere in
  match
    resolve (
      after (Date_time.now ())
      & months [`Dec]
      & days [25]
      & weekdays [`Wed]
    )
  with
  | Error msg -> failwith msg
  | Ok s ->
    Fmt.pr "%a@." (pp_intervals ~sep:(Fmt.any "@.") ()) s
