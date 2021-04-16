open Date_time_components

module Print_utils = struct
  let small_nat = QCheck.Print.int

  let int64 = Int64.to_string

  let span = Printers.string_of_span

  let duration = Printers.string_of_duration

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
    (fun (days, hours, (minutes, seconds, ns)) ->
       Duration.make ~days ~hours ~minutes ~seconds ~ns ())
    (triple nat nat (triple nat nat nat))

let duration = QCheck.make ~print:Print_utils.duration duration_gen

let timestamp_bound_gen bound =
  let open QCheck.Gen in
  map
    (fun (pos, s, ns) ->
       let t =
         Span.(
           make ~s ~ns ()
           |> max zero
           |> min bound
           |> fun x -> if pos then x else max Time.timestamp_min (neg x))
       in
       t)
    (triple bool (int64_bound_gen Time.timestamp_max.s) big_nat)

let pos_timestamp_bound_gen bound =
  QCheck.Gen.(
    map
      (fun (s, ns) -> Span.(make ~s ~ns () |> max zero |> min bound))
      (pair (int64_bound_gen Time.timestamp_max.s) big_nat))

let nz_pos_timestamp_bound_gen bound =
  QCheck.Gen.(
    map
      (fun (s, ns) -> Span.(make ~s ~ns () |> max (make ~ns:1 ()) |> min bound))
      (pair ui64 int))

let small_pos_timestamp_gen = pos_timestamp_bound_gen (Span.make ~s:100L ())

let small_nz_pos_timestamp_gen =
  nz_pos_timestamp_bound_gen (Span.make ~s:100L ())

let timestamp_gen = timestamp_bound_gen Time.timestamp_max

let pos_timestamp_gen = pos_timestamp_bound_gen Time.timestamp_max

let nz_pos_timestamp_gen = nz_pos_timestamp_bound_gen Time.timestamp_max

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
              | Some x -> Span.add x gap
            in
            let end_exc = Span.add start size in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair
       (timestamp_bound_gen (Span.make ~s:10_000L ()))
       (list_size (int_bound 5)
          (pair
             (pos_timestamp_bound_gen Span.(make ~s:20L ()))
             (pos_timestamp_bound_gen (Span.make ~s:20L ())))))

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
              | Some x -> Span.add x gap
            in
            let end_exc = Span.add start size in
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
              | Some x -> Span.add x gap
            in
            let end_exc = Span.add start size in
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
                let start = Span.add last_start gap in
                let size =
                  if start = last_start then Span.add last_size size
                  else size
                in
                (start, size)
            in
            let end_exc = Span.add start size in
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
    (List.map (fun (start, size) -> (start, Span.add start size)))
    (list_size (int_bound 5)
       (pair
          (timestamp_bound_gen (Span.make ~s:10_000L ()))
          (pos_timestamp_bound_gen (Span.make ~s:20L ()))))

let tiny_time_slots =
  QCheck.make ~print:Print_utils.time_slots tiny_time_slots_gen

let time_slots_gen =
  let open QCheck.Gen in
  map
    (List.map (fun (start, size) -> (start, Span.add start size)))
    (list_size (int_bound 100)
       (pair
          (timestamp_bound_gen (Span.make ~s:100_000L ()))
          small_pos_timestamp_gen))

let time_slots = QCheck.make ~print:Print_utils.time_slots time_slots_gen

let weekday_gen : weekday QCheck.Gen.t =
  QCheck.Gen.(oneofl [ `Sun; `Mon; `Tue; `Wed; `Thu; `Fri; `Sat ])

let month_gen : month QCheck.Gen.t =
  let open QCheck.Gen in
  oneofl
    [ `Jan; `Feb; `Mar; `Apr; `May; `Jun; `Jul; `Aug; `Sep; `Oct; `Nov; `Dec ]

let month_days_gen : int list QCheck.Gen.t =
  QCheck.Gen.(list_size (int_bound 10) (int_range 1 32))

let month_days = QCheck.make ~print:QCheck.Print.(list int) month_days_gen

let weekdays_gen : weekday list QCheck.Gen.t =
  QCheck.Gen.(list_size (int_bound 10) weekday_gen)

let weekdays =
  QCheck.make
    ~print:(QCheck.Print.list Time.abbr_string_of_weekday)
    weekdays_gen

(* let time_pattern_gen : Time_pattern.time_pattern QCheck.Gen.t =
 *   let open QCheck.Gen in
 *   map
 *     (fun (years, months, month_days, (weekdays, hours, minutes, seconds)) ->
 *        let open Daypack_lib.Time_pattern in
 *        {
 *          years;
 *          months;
 *          month_days;
 *          weekdays;
 *          hours;
 *          minutes;
 *          seconds;
 *          unix_seconds = [];
 *        })
 *     (quad
 *        (list_size (int_bound 5) (int_range 1980 2100))
 *        (list_size (int_bound 5) month_gen)
 *        month_days_gen
 *        (quad weekdays_gen
 *           (list_size (int_bound 5) (int_bound 24))
 *           (list_size (int_bound 5) (int_bound 60))
 *           (list_size (int_bound 5) (int_bound 60)))) *)

let default_date_time_format_string =
  "{year} {mon:Xxx} {mday:0X} {wday:Xxx} {hour:0X}:{min:0X}:{sec:0X}"

let default_interval_format_string =
  "[{syear} {smon:Xxx} {smday:0X} {swday:Xxx} {shour:0X}:{smin:0X}:{ssec:0X}, \
   {eyear} {emon:Xxx} {emday:0X} {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X})"

let span_testable : (module Alcotest.TESTABLE with type t = Span.t) =
  (module struct
    type t = Span.t

    let pp = Printers.pp_span

    let equal = Span.equal
  end)

let date_time_testable : (module Alcotest.TESTABLE) =
  (module struct
    type t = Time.Date_time'.t

    let pp formatter t = Printers.pp_date_time () formatter t

    let equal = Time.Date_time'.equal
  end)

let tz_testable : (module Alcotest.TESTABLE with type t = Time_zone.t) =
  (module struct
    type t = Time_zone.t

    let pp _formatter _t = failwith "Time zone is not printable"

    let equal = Time_zone.equal
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

let time_gen : Time_ast.t QCheck.Gen.t =
  let open QCheck.Gen in
  let search_start_dt =
    CCOpt.get_exn
    @@ Time.Date_time'.make ~year:2018 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz:Time_zone.utc ()
  in
  let search_start = Time.Date_time'.to_timestamp_single search_start_dt in
  let search_end_exc_dt =
    CCOpt.get_exn
    @@ Time.Date_time'.make ~year:2021 ~month:`Jan ~day:1 ~hour:0 ~minute:0
      ~second:0 ~tz:Time_zone.utc ()
  in
  let search_end_exc = Time.Date_time'.to_timestamp_single search_end_exc_dt in
  map3
    (fun max_height max_branching randomness ->
       Time.inter
         [
           Time.intervals [ (search_start, search_end_exc) ];
           Builder.build ~enable_extra_restrictions:true ~min_year:2018
             ~max_year_inc:2020 ~max_height ~max_branching ~randomness;
         ])
    (int_range 1 2) (int_range 1 3)
    (list_size (int_bound 10) (int_bound 100))

let time = QCheck.make ~print:To_sexp.to_sexp_string time_gen

let time_list_gen n : Time_ast.t list QCheck.Gen.t =
  let open QCheck.Gen in
  list_size (int_bound n) time_gen

let time_list n =
  QCheck.make
    ~print:(fun l -> String.concat ", " (List.map To_sexp.to_sexp_string l))
    (time_list_gen n)

let time_zone_gen : Time_zone.t QCheck.Gen.t =
  let open QCheck.Gen in
  let tz_count = List.length Time_zone.available_time_zones in
  map
    (fun n -> Time_zone.make_exn (List.nth Time_zone.available_time_zones n))
    (int_bound (tz_count - 1))

let time_zone =
  QCheck.make ~print:(fun (t : Time_zone.t) -> t.name) time_zone_gen

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
