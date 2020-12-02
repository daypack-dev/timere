open Test_utils

let search_start_dt =
  Result.get_ok
  @@ Time.Date_time.make ~year:2000 ~month:`Jan ~day:1 ~hour:0 ~minute:0
    ~second:0 ~tz_offset_s:0

let search_start = Time.Date_time.to_timestamp search_start_dt

let search_end_exc_dt =
  Result.get_ok
  @@ Time.Date_time.make ~year:2001 ~month:`Jan ~day:1 ~hour:0 ~minute:0
    ~second:0 ~tz_offset_s:0

let search_end_exc = Time.Date_time.to_timestamp search_end_exc_dt

module Qc = struct
  let resolver_is_same_as_simple_resolver =
    QCheck.Test.make ~count:1000 ~name:"resolver_is_same_as_simple_resolver"
      time (fun t ->
          OSeq.equal ~eq:( = )
            ( Result.get_ok
              @@ Resolver.resolve
                Time.(
                  inter t (interval_dt_exc search_start_dt search_end_exc_dt)) )
            (Simple_resolver.resolve ~search_start ~search_end_exc ~tz_offset_s:0
               t))

  let suite = [ resolver_is_same_as_simple_resolver ]
end
