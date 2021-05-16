module Print_utils = struct
  let small_nat = QCheck.Print.int

  let int64 = Int64.to_string

  let span = Timedesc.Span.to_string

  let duration = Timedesc.Span.For_human.to_string

  let time_slot = QCheck.Print.pair span span

  let time_slots = QCheck.Print.list time_slot
end

let nz_small_nat_gen = QCheck.Gen.(map (( + ) 1) small_nat)

let nz_small_nat = QCheck.make nz_small_nat_gen

let int64_bound_gen bound =
  let open QCheck.Gen in
  map
    (fun (pos, x) ->
       x |> max 0L |> min bound |> fun x -> if pos then x else Int64.mul (-1L) x)
    (pair bool ui64)

let pos_int64_bound_gen bound =
  QCheck.Gen.(map (fun x -> x |> max 0L |> min bound) ui64)

let nz_pos_int64_bound_gen bound =
  QCheck.Gen.(map (fun x -> x |> max 1L |> min bound) ui64)

let small_pos_int64_gen = pos_int64_bound_gen 100L

let small_nz_pos_int64_gen = nz_pos_int64_bound_gen 100L

let int64_gen = int64_bound_gen (Int64.sub Int64.max_int 1L)

let pos_int64_gen = pos_int64_bound_gen (Int64.sub Int64.max_int 1L)

let pos_int64 = QCheck.make ~print:Print_utils.int64 pos_int64_gen

let small_pos_int64 = QCheck.make ~print:Print_utils.int64 small_pos_int64_gen

let small_nz_pos_int64 =
  QCheck.make ~print:Print_utils.int64 small_nz_pos_int64_gen

let nz_pos_int64_gen =
  QCheck.Gen.map (Int64.add 1L)
    (pos_int64_bound_gen (Int64.sub Int64.max_int 1L))

let nz_pos_int64 = QCheck.make ~print:Print_utils.int64 nz_pos_int64_gen

let pos_int64_int64_option_bound_gen bound =
  QCheck.Gen.(
    pair (pos_int64_bound_gen bound) (opt (pos_int64_bound_gen bound)))

let nz_pos_int64_int64_option_bound_gen bound =
  let open QCheck.Gen in
  pair (nz_pos_int64_bound_gen bound) (opt (nz_pos_int64_bound_gen bound))

let small_pos_int64_int64_option_gen =
  QCheck.Gen.(pair small_pos_int64_gen (opt small_pos_int64_gen))

let small_nz_pos_int64_int64_option_gen =
  QCheck.Gen.(pair small_nz_pos_int64_gen (opt small_nz_pos_int64_gen))

let pos_int64_int64_option_gen =
  QCheck.Gen.(pair pos_int64_gen (opt pos_int64_gen))

let nz_pos_int64_int64_option_gen =
  nz_pos_int64_int64_option_bound_gen (Int64.sub Int64.max_int 1L)

let duration_gen =
  let open QCheck.Gen in
  map
    (fun (pos, days, hours, (minutes, seconds, ns)) ->
       let sign = if pos then `Pos else `Neg in
       Timedesc.Span.For_human.make_exn ~sign ~days ~hours ~minutes ~seconds ~ns
         ())
    (quad bool nat nat (triple nat nat nat))

let duration = QCheck.make ~print:Print_utils.duration duration_gen

let timestamp_bound_gen bound =
  let open QCheck.Gen in
  map
    (fun (pos, s, ns) ->
       let t =
         Timedesc.Span.(
           make ~s ~ns ()
           |> max zero
           |> min bound
           |> fun x -> if pos then x else max Timedesc.Timestamp.min_val (neg x))
       in
       t)
    (triple bool
       (int64_bound_gen (Int64.pred Timedesc.Timestamp.max_val.s))
       (int_bound 1_000_000_000))

let pos_timestamp_bound_gen bound =
  QCheck.Gen.(
    map
      (fun (s, ns) -> Timedesc.Span.(make ~s ~ns () |> max zero |> min bound))
      (pair
         (int64_bound_gen (Int64.pred Timedesc.Timestamp.max_val.s))
         (int_bound 1_000_000_000)))

let nz_pos_timestamp_bound_gen bound =
  QCheck.Gen.(
    map
      (fun (s, ns) ->
         Timedesc.Span.(make ~s ~ns () |> max (make ~ns:1 ()) |> min bound))
      (pair ui64 int))

let small_pos_timestamp_gen =
  pos_timestamp_bound_gen (Timedesc.Span.make ~s:100L ())

let small_nz_pos_timestamp_gen =
  nz_pos_timestamp_bound_gen (Timedesc.Span.make ~s:100L ())

let timestamp_gen = timestamp_bound_gen Timedesc.Timestamp.max_val

let pos_timestamp_gen = pos_timestamp_bound_gen Timedesc.Timestamp.max_val

let nz_pos_timestamp_gen = nz_pos_timestamp_bound_gen Timedesc.Timestamp.max_val

let pos_timestamp = QCheck.make ~print:Print_utils.span pos_timestamp_gen

let small_pos_timestamp =
  QCheck.make ~print:Print_utils.span small_pos_timestamp_gen

let small_nz_pos_timestamp =
  QCheck.make ~print:Print_utils.span small_nz_pos_timestamp_gen

let timestamp = QCheck.make ~print:Print_utils.span timestamp_gen

let nz_pos_timestamp = QCheck.make ~print:Print_utils.span nz_pos_timestamp_gen

let tiny_sorted_time_slots_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Timedesc.Span.add x gap
            in
            let end_exc = Timedesc.Span.add start size in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair
       (timestamp_bound_gen (Timedesc.Span.make ~s:10_000L ()))
       (list_size (int_bound 5)
          (pair
             (pos_timestamp_bound_gen Timedesc.Span.(make ~s:20L ()))
             (pos_timestamp_bound_gen (Timedesc.Span.make ~s:20L ())))))

let tiny_sorted_time_slots =
  QCheck.make ~print:Print_utils.time_slots tiny_sorted_time_slots_gen

let sorted_time_slots_maybe_gaps_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Timedesc.Span.add x gap
            in
            let end_exc = Timedesc.Span.add start size in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair timestamp_gen
       (list_size (int_bound 1000)
          (pair small_nz_pos_timestamp_gen small_pos_timestamp_gen)))

let sorted_time_slots_maybe_gaps =
  QCheck.make ~print:Print_utils.time_slots sorted_time_slots_maybe_gaps_gen

let sorted_time_slots_with_gaps_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Timedesc.Span.add x gap
            in
            let end_exc = Timedesc.Span.add start size in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair timestamp_gen
       (list_size (int_bound 1000)
          (pair small_nz_pos_timestamp_gen small_nz_pos_timestamp_gen)))

let sorted_time_slots_with_gaps =
  QCheck.make ~print:Print_utils.time_slots sorted_time_slots_with_gaps_gen

let sorted_time_slots_with_overlaps_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_start_and_size, acc) (size, gap) ->
            let start, size =
              match last_start_and_size with
              | None -> (start, size)
              | Some (last_start, last_size) ->
                let start = Timedesc.Span.add last_start gap in
                let size =
                  if start = last_start then Timedesc.Span.add last_size size
                  else size
                in
                (start, size)
            in
            let end_exc = Timedesc.Span.add start size in
            (Some (start, size), (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair timestamp_gen
       (list_size (int_bound 1000)
          (pair small_nz_pos_timestamp_gen small_pos_timestamp_gen)))

let sorted_time_slots_with_overlaps =
  QCheck.make ~print:Print_utils.time_slots sorted_time_slots_with_overlaps_gen

let tiny_time_slots_gen =
  let open QCheck.Gen in
  map
    (List.map (fun (start, size) -> (start, Timedesc.Span.add start size)))
    (list_size (int_bound 5)
       (pair
          (timestamp_bound_gen (Timedesc.Span.make ~s:10_000L ()))
          (pos_timestamp_bound_gen (Timedesc.Span.make ~s:20L ()))))

let tiny_time_slots =
  QCheck.make ~print:Print_utils.time_slots tiny_time_slots_gen

let time_slots_gen =
  let open QCheck.Gen in
  map
    (List.map (fun (start, size) -> (start, Timedesc.Span.add start size)))
    (list_size (int_bound 100)
       (pair
          (timestamp_bound_gen (Timedesc.Span.make ~s:100_000L ()))
          small_pos_timestamp_gen))

let time_slots = QCheck.make ~print:Print_utils.time_slots time_slots_gen

let weekday_gen : Timedesc.weekday QCheck.Gen.t =
  QCheck.Gen.(oneofl [ `Sun; `Mon; `Tue; `Wed; `Thu; `Fri; `Sat ])

let month_gen : int QCheck.Gen.t =
  let open QCheck.Gen in
  map succ (int_bound 12)

let month_days_gen : int list QCheck.Gen.t =
  QCheck.Gen.(list_size (int_bound 10) (int_range 1 32))

let month_days = QCheck.make ~print:QCheck.Print.(list int) month_days_gen

let weekdays_gen : Timedesc.weekday list QCheck.Gen.t =
  QCheck.Gen.(list_size (int_bound 10) weekday_gen)

let weekdays =
  QCheck.make
    ~print:(QCheck.Print.list Timedesc.Utils.abbr_string_of_weekday)
    weekdays_gen

let span_testable : (module Alcotest.TESTABLE with type t = Timedesc.Span.t) =
  (module struct
    type t = Timedesc.Span.t

    let pp = Timedesc.Span.pp

    let equal = Timedesc.Span.equal
  end)

let date_time_testable : (module Alcotest.TESTABLE) =
  (module struct
    type t = Timedesc.t

    let pp formatter t = Timedesc.pp () formatter t

    let equal = Timedesc.equal
  end)

let tz_testable : (module Alcotest.TESTABLE with type t = Timedesc.Time_zone.t)
  =
  (module struct
    type t = Timedesc.Time_zone.t

    let pp _formatter _t = failwith "Time zone is not printable"

    let equal = Timedesc.Time_zone.equal
  end)

(* let time_pattern_testable : (module Alcotest.TESTABLE) =
 *   ( module struct
 *     type t = Daypack_lib.Time_pattern.time_pattern
 * 
 *     let pp =
 *       Fmt.using Daypack_lib.Time_pattern.To_string.debug_string_of_time_pattern
 *         Fmt.string
 * 
 *     let equal = ( = )
 *   end ) *)

let time_zone_gen : Timedesc.Time_zone.t QCheck.Gen.t =
  let open QCheck.Gen in
  let tz_count = List.length Timedesc.Time_zone.available_time_zones in
  map
    (fun n ->
       Timedesc.Time_zone.make_exn
         (List.nth Timedesc.Time_zone.available_time_zones n))
    (int_bound (tz_count - 1))

let time_zone =
  QCheck.make
    ~print:(fun (t : Timedesc.Time_zone.t) -> Timedesc.Time_zone.name t)
    time_zone_gen

let permute (seed : int) (l : 'a list) : 'a list =
  let len = List.length l in
  let l = ref l in
  OSeq.(0 --^ len)
  |> Seq.map (fun i ->
      let l' = List.mapi (fun i x -> (i, x)) !l in
      let len = List.length l' in
      let pick = i * seed mod len in
      let r = List.assoc pick l' in
      l := List.remove_assoc pick l' |> List.map (fun (_, x) -> x);
      r)
  |> CCList.of_seq

let iso_ord_date_gen : (int * int) QCheck.Gen.t =
  let open QCheck.Gen in
  map2
    (fun year day_of_year ->
       let day_of_year = Int64.to_int day_of_year in
       let day_of_year =
         if Timedesc.Utils.is_leap_year ~year then day_of_year + 1
         else (day_of_year mod 365) + 1
       in
       (year, day_of_year))
    (int_range 1 9998) (pos_int64_bound_gen 365L)

let iso_ord_date =
  QCheck.make
    ~print:(fun (year, day_of_year) ->
        Printf.sprintf "%d-%03d" year day_of_year)
    iso_ord_date_gen

let iso_week_date_gen : (int * int * Timedesc.weekday) QCheck.Gen.t =
  let open QCheck.Gen in
  map3
    (fun iso_week_year week weekday ->
       let week =
         Int64.to_int week
         mod Timedesc.Utils.week_count_of_iso_week_year ~iso_week_year
         + 1
       in
       (iso_week_year, week, weekday))
    (int_range 1 9998) (pos_int64_bound_gen 53L) weekday_gen

let iso_week_date =
  QCheck.make
    ~print:(fun (iso_week_year, week, weekday) ->
        Printf.sprintf "%d-%02d-%s" iso_week_year week
          (Timedesc.Utils.abbr_string_of_weekday weekday))
    iso_week_date_gen

let ymd_date_gen : (int * int * int) QCheck.Gen.t =
  let open QCheck.Gen in
  map3
    (fun year month day ->
       let month = Int64.to_int month + 1 in
       let day =
         (Int64.to_int day mod Timedesc.Utils.day_count_of_month ~year ~month)
         + 1
       in
       (year, month, day))
    (int_range 1 9998) (pos_int64_bound_gen 11L) (pos_int64_bound_gen 30L)

let ymd_date =
  QCheck.make
    ~print:(fun (year, month, day) ->
        Printf.sprintf "%d-%02d-%02d" year month day)
    ymd_date_gen

let time_gen : (int * int * int * int) QCheck.Gen.t =
  let open QCheck.Gen in
  map3
    (fun hour minute (second, ns) -> (hour, minute, second, ns))
    (int_bound 23) (int_bound 59)
    (pair (int_bound 59) (int_bound (pred Timedesc.Span.ns_count_in_s)))

let time =
  QCheck.make
    ~print:(fun (hour, minute, second, ns) ->
        Printf.sprintf "%d:%d:%d_%d" hour minute second ns)
    time_gen

let date_time_gen =
  let open QCheck.Gen in
  map2
    (fun timestamp tz_of_date_time ->
       Timedesc.of_timestamp_exn ~tz_of_date_time timestamp)
    timestamp_gen time_zone_gen

let date_time =
  QCheck.make
    ~print:(fun dt ->
        dt
        |> Timedesc.to_string
        |> CCOpt.get_exn_or
          "Expected successful construction of string from timedesc object")
    date_time_gen

let ptime_gen : Ptime.t QCheck.Gen.t =
  let open QCheck.Gen in
  map2
    (fun (year, month, day) (hour, minute, second, _ns) ->
       CCOpt.get_exn_or "Expected successful construction of ptime"
       @@ Ptime.of_date_time ((year, month, day), ((hour, minute, second), 0)))
    ymd_date_gen time_gen

let ptime = QCheck.make ~print:Ptime.to_rfc3339 ptime_gen
