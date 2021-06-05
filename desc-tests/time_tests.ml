open Test_utils

module Alco = struct
  let end_of_day_24_00_00 () =
    Alcotest.(check span_testable)
      "same span"
      (Timedesc.Time.make_exn ~hour:24 ~minute:0 ~second:0 ~ns:0 ()
      |> Timedesc.Time.to_span)
      (Timedesc.Time.make_exn ~hour:23 ~minute:59 ~second:59 ~ns:999_999_999 ()
      |> Timedesc.Time.to_span)

  let suite =
    [ Alcotest.test_case "end_of_day_24_00_00" `Quick end_of_day_24_00_00 ]
end

module Qc = struct
  let accessors =
    QCheck.Test.make ~count:100_000 ~name:"accessors" time
      (fun (hour, minute, second, ns) ->
        let time = Timedesc.Time.make_exn ~hour ~minute ~second ~ns () in
        let hour' = Timedesc.Time.hour time in
        let minute' = Timedesc.Time.minute time in
        let second' = Timedesc.Time.second time in
        let ns' = Timedesc.Time.ns time in
        hour = hour' && minute = minute' && second = second' && ns = ns')

  let suite = [ accessors ]
end
