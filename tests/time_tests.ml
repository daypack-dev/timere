open Test_utils

let search_start_dt =
  Result.get_ok
  @@ Time.Date_time.make ~year:2000 ~month:`Jan ~day:1 ~hour:0 ~minute:0
    ~second:0 ~tz:Time_zone.utc

let search_start =
  Time.Date_time.to_timestamp search_start_dt
  |> Time.Date_time.min_of_timestamp_local_result
  |> Option.get

let search_end_exc_dt =
  Result.get_ok
  @@ Time.Date_time.make ~year:2003 ~month:`Jan ~day:1 ~hour:0 ~minute:0
    ~second:0 ~tz:Time_zone.utc

let search_end_exc =
  Time.Date_time.to_timestamp search_end_exc_dt
  |> Time.Date_time.max_of_timestamp_local_result
  |> Option.get

module Qc = struct
  let resolver_is_same_as_simple_resolver =
    QCheck.Test.make ~count:1 ~name:"resolver_is_same_as_simple_resolver" time
      (fun t ->
         OSeq.equal ~eq:( = )
           (Result.get_ok
            @@ Resolver.resolve
              Time.(
                inter [ t; interval_dt_exc search_start_dt search_end_exc_dt ])
           )
           (Simple_resolver.resolve ~search_start ~search_end_exc
              ~search_using_tz:Time_zone.utc t))

  let to_of_sexp =
    QCheck.Test.make ~count:10_000 ~name:"to_of_sexp" time (fun t ->
        let t' = t |> To_sexp.to_sexp |> Of_sexp.of_sexp |> Result.get_ok in
        Time.equal t t')

  let union_order_does_not_matter =
    QCheck.Test.make ~count:10_000 ~name:"union_order_does_not_matter" time_list
      (fun l1 ->
         let l2 = List.rev l1 in
         let l3 =
           let x = l1 |> List.to_seq |> OSeq.take_nth 2 |> List.of_seq in
           let y =
             l1 |> List.to_seq |> OSeq.drop 1 |> OSeq.take_nth 2 |> List.of_seq
           in
           x @ y
         in
         let t1 = Time.union l1 in
         let t2 = Time.union l2 in
         let t3 = Time.union l3 in
         let r1 = Result.get_ok @@ Resolver.resolve t1 in
         let r2 = Result.get_ok @@ Resolver.resolve t2 in
         let r3 = Result.get_ok @@ Resolver.resolve t3 in
         OSeq.equal ~eq:( = ) r1 r2 && OSeq.equal ~eq:( = ) r2 r3)

  let inter_order_does_not_matter =
    QCheck.Test.make ~count:10_000 ~name:"inter_order_does_not_matter" time_list
      (fun l1 ->
         let l2 = List.rev l1 in
         let l3 =
           let x = l1 |> List.to_seq |> OSeq.take_nth 2 |> List.of_seq in
           let y =
             l1 |> List.to_seq |> OSeq.drop 1 |> OSeq.take_nth 2 |> List.of_seq
           in
           x @ y
         in
         let t1 = Time.inter l1 in
         let t2 = Time.inter l2 in
         let t3 = Time.inter l3 in
         let r1 = Result.get_ok @@ Resolver.resolve t1 in
         let r2 = Result.get_ok @@ Resolver.resolve t2 in
         let r3 = Result.get_ok @@ Resolver.resolve t3 in
         OSeq.equal ~eq:( = ) r1 r2 && OSeq.equal ~eq:( = ) r2 r3)

  let suite =
    [
      (* resolver_is_same_as_simple_resolver; *)
      to_of_sexp;
      union_order_does_not_matter;
      inter_order_does_not_matter;
    ]
end
