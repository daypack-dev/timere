open Test_utils

module Qc = struct
  let to_of_json =
    QCheck.Test.make ~count:100_000 ~name:"to_of_json" time_zone (fun tz ->
        let tz' =
          tz
          |> Time_zone.JSON.to_json
          |> Time_zone.JSON.of_json
          |> CCOpt.get_exn
        in
        Time_zone.equal tz tz')

  let suite = [ to_of_json ]
end
