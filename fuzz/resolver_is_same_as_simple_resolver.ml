open Fuzz_utils

let search_start_dt =
  Timedesc.make_exn ~year:2000 ~month:1 ~day:1 ~hour:0 ~minute:0 ~second:0
    ~tz:Timedesc.Time_zone.utc ()

let search_start =
  Timedesc.to_timestamp search_start_dt |> Timedesc.min_of_local_result

let search_end_exc_dt =
  Timedesc.make_exn ~year:2003 ~month:1 ~day:1 ~hour:0 ~minute:0 ~second:0
    ~tz:Timedesc.Time_zone.utc ()

let search_end_exc =
  Timedesc.to_timestamp search_end_exc_dt |> Timedesc.max_of_local_result

let () =
  Crowbar.add_test ~name:"resolver_is_same_as_simple_resolver" [ time ]
    (fun t ->
      let r =
        OSeq.equal ~eq:Time.Interval'.equal
          (CCResult.get_exn
          @@ Resolver.resolve
               Time.(inter [ t; intervals [ (search_start, search_end_exc) ] ])
          )
          (Simple_resolver.resolve ~search_start ~search_end_exc
             ~search_using_tz:Timedesc.Time_zone.utc t)
      in
      if not r then Crowbar.fail (Fmt.str "%a\n" Printers.pp_sexp t))
