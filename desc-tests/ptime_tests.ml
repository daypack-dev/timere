open Test_utils

module Qc = struct
  let span_to_of_ptime_span =
    QCheck.Test.make ~count:100_000 ~name:"span_to_of_ptime_span" timestamp
      (fun s ->
        let s' =
          s
          |> Timedesc.Utils.ptime_span_of_span
          |> CCOpt.get_exn_or
               "Expected successful construction of Ptime.span from span"
          |> Timedesc.Utils.span_of_ptime_span
        in
        Timedesc.Span.equal s s')

  let timestamp_to_of_ptime =
    QCheck.Test.make ~count:100_000 ~name:"timestamp_to_of_ptime" timestamp
      (fun s ->
        let s' =
          s
          |> Timedesc.Utils.ptime_of_timestamp
          |> CCOpt.get_exn_or
               "Expected successful construction of ptime from timestamp"
          |> Timedesc.Utils.timestamp_of_ptime
        in
        Timedesc.Span.equal s s')

  let suite = [ span_to_of_ptime_span; timestamp_to_of_ptime ]
end
