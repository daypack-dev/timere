open Test_utils

module Qc = struct
  let to_of_compressed =
    QCheck.Test.make ~count:100_000 ~name:"to_of_compressed" time_zone (fun tz ->
        let tz' =
          tz
          |> Timedesc.Time_zone.Compressed.to_string
          |> Timedesc.Time_zone.Compressed.of_string
          |> CCOption.get_exn_or
            "Expected successful construction of time zone from compressed string"
        in
        Timedesc.Time_zone.equal tz tz')

  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" time_zone (fun tz ->
        let tz' =
          tz
          |> Timedesc_sexp.Time_zone.to_sexp
          |> Timedesc_sexp.Time_zone.of_sexp
          |> CCOption.get_exn_or
            "Expected successful construction of time zone from sexp"
        in
        Timedesc.Time_zone.equal tz tz')

  let to_of_json =
    QCheck.Test.make ~count:100_000 ~name:"to_of_json" time_zone (fun tz ->
        let tz' =
          tz
          |> Timedesc_json.Time_zone.to_json
          |> Timedesc_json.Time_zone.of_json
          |> CCOption.get_exn_or
            "Expected successful construction of time zone from JSOn"
        in
        Timedesc.Time_zone.equal tz tz')

  let suite =
    [
      to_of_compressed;
      to_of_sexp;
      to_of_json;
    ]
end
