open Test_utils

let search_start_dt =
  Result.get_ok
  @@ Time.Date_time.make ~year:2000 ~month:`Jan ~day:1 ~hour:0 ~minute:0
    ~second:0 ~tz_offset_s:0

let search_start = Time.Date_time.to_timestamp search_start_dt

let search_end_exc_dt =
  Result.get_ok
  @@ Time.Date_time.make ~year:2003 ~month:`Jan ~day:1 ~hour:0 ~minute:0
    ~second:0 ~tz_offset_s:0

let search_end_exc = Time.Date_time.to_timestamp search_end_exc_dt

module Qc = struct
  let resolver_is_same_as_simple_resolver =
    QCheck.Test.make ~count:5 ~name:"resolver_is_same_as_simple_resolver" time
      (fun t ->
         OSeq.equal ~eq:( = )
           (Result.get_ok
            @@ Resolver.resolve
              Time.(
                inter [ t; interval_dt_exc search_start_dt search_end_exc_dt ])
           )
           (Simple_resolver.resolve ~search_start ~search_end_exc ~tz_offset_s:0
              t))

  let to_of_sexp =
    QCheck.Test.make ~count:100_000 ~name:"to_of_sexp" time (fun t ->
        let t' = t |> To_sexp.to_sexp |> Of_sexp.of_sexp |> Result.get_ok in
        Time.equal t t')

  let suite = [ resolver_is_same_as_simple_resolver; to_of_sexp ]
end
