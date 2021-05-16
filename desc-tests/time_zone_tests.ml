open Test_utils

module Qc = struct
  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" time_zone (fun tz ->
        let tz' =
          tz
          |> Timedesc.Time_zone.Sexp.to_sexp
          |> Timedesc.Time_zone.Sexp.of_sexp
          |> CCOpt.get_exn_or
            "Expected successful construction of time zone from sexp"
        in
        Timedesc.Time_zone.equal tz tz')

  let to_of_json =
    QCheck.Test.make ~count:100_000 ~name:"to_of_json" time_zone (fun tz ->
        let tz' =
          tz
          |> Timedesc.Time_zone.JSON.to_json
          |> Timedesc.Time_zone.JSON.of_json
          |> CCOpt.get_exn_or
            "Expected successful construction of time zone from JSOn"
        in
        Timedesc.Time_zone.equal tz tz')

  let suite = [ to_of_sexp; to_of_json ]
end
