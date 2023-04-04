open Test_utils

module Alco = struct
  let suite = []
end

module Qc = struct
  let to_string_does_not_crash =
    QCheck.Test.make ~count:100_000 ~name:"to_string_does_not_crash"
      QCheck.(pair timestamp timestamp) (fun (ts1, ts2) ->
          Timedesc.Interval.to_string (ts1, ts2) |> ignore;
          true
        )

  let suite =
    [
      to_string_does_not_crash;
    ]
end
