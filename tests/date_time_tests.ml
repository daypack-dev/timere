open Test_utils

module Qc = struct
  let to_rfc3339_nano_of_iso8601_is_lossless =
    QCheck.Test.make ~count:100_000
      ~name:"to_rfc3339_nano_of_iso8601_is_lossless" timestamp (fun timestamp ->
          let r =
            CCResult.get_exn
            @@ ISO8601.to_timestamp
            @@ RFC3339.of_timestamp ~precision:9 timestamp
          in
          Span.equal r timestamp)

  let suite = [ to_rfc3339_nano_of_iso8601_is_lossless ]
end
