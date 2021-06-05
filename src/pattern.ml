type t = {
  years : Int_set.t;
  months : Int_set.t;
  month_days : Int_set.t;
  weekdays : Weekday_set.t;
  hours : Int_set.t;
  minutes : Int_set.t;
  seconds : Int_set.t;
  ns : Diet.Int.t;
}

let set_of_int_seq s =
  Seq.fold_left (fun acc x -> Int_set.add x acc) Int_set.empty s

let full_hours = set_of_int_seq OSeq.(0 -- 23)

let full_minutes = set_of_int_seq OSeq.(0 -- 59)

let full_seconds = set_of_int_seq OSeq.(0 -- 59)

let full_ns =
  Diet.Int.add
    (Diet.Int.Interval.make 0 (Timedesc.Span.ns_count_in_s - 1))
    Diet.Int.empty

let optimize_hours (t : t) : t =
  if Int_set.equal t.hours full_hours then { t with hours = Int_set.empty }
  else t

let optimize_minutes (t : t) : t =
  if Int_set.equal t.minutes full_minutes then
    { t with minutes = Int_set.empty }
  else t

let optimize_seconds (t : t) : t =
  if Int_set.equal t.seconds full_seconds then
    { t with seconds = Int_set.empty }
  else t

let optimize_ns (t : t) : t =
  if Diet.Int.equal t.ns full_ns then { t with ns = Diet.Int.empty } else t

let optimize (t : t) : t =
  t |> optimize_hours |> optimize_minutes |> optimize_seconds |> optimize_ns

let equal p1 p2 =
  Int_set.equal p1.years p2.years
  && Int_set.equal p1.months p2.months
  && Int_set.equal p1.month_days p2.month_days
  && Weekday_set.equal p1.weekdays p2.weekdays
  && Int_set.equal p1.hours p2.hours
  && Int_set.equal p1.minutes p2.minutes
  && Int_set.equal p1.seconds p2.seconds
  && Diet.Int.equal p1.ns p2.ns

let union p1 p2 =
  let union_sets (type a) ~(is_empty : a -> bool) ~(union : a -> a -> a)
      ~(empty : a) (a : a) (b : a) =
    if is_empty a || is_empty b then empty else union a b
  in
  let union_int_sets a b =
    union_sets ~is_empty:Int_set.is_empty ~union:Int_set.union
      ~empty:Int_set.empty a b
  in
  let union_weekday_sets a b =
    union_sets ~is_empty:Weekday_set.is_empty ~union:Weekday_set.union
      ~empty:Weekday_set.empty a b
  in
  {
    years = union_int_sets p1.years p2.years;
    months = union_int_sets p1.months p2.months;
    month_days = union_int_sets p1.month_days p2.month_days;
    weekdays = union_weekday_sets p1.weekdays p2.weekdays;
    hours = union_int_sets p1.hours p2.hours;
    minutes = union_int_sets p1.minutes p2.minutes;
    seconds = union_int_sets p1.seconds p2.seconds;
    ns =
      union_sets ~is_empty:Diet.Int.is_empty ~union:Diet.Int.union
        ~empty:Diet.Int.empty p1.ns p2.ns;
  }

let inter p1 p2 =
  let inter_sets (type a) ~(is_empty : a -> bool) ~(inter : a -> a -> a) (a : a)
      (b : a) =
    if is_empty a then Some b
    else if is_empty b then Some a
    else
      let s = inter a b in
      if is_empty s then None else Some s
  in
  let inter_int_sets a b =
    inter_sets ~is_empty:Int_set.is_empty ~inter:Int_set.inter a b
  in
  let inter_weekday_sets a b =
    inter_sets ~is_empty:Weekday_set.is_empty ~inter:Weekday_set.inter a b
  in
  let inter_ns_sets a b =
    inter_sets ~is_empty:Diet.Int.is_empty ~inter:Diet.Int.inter a b
  in
  match inter_int_sets p1.years p2.years with
  | None -> None
  | Some years -> (
      match inter_int_sets p1.months p2.months with
      | None -> None
      | Some months -> (
          match inter_int_sets p1.month_days p2.month_days with
          | None -> None
          | Some month_days -> (
              match inter_weekday_sets p1.weekdays p2.weekdays with
              | None -> None
              | Some weekdays -> (
                  match inter_int_sets p1.hours p2.hours with
                  | None -> None
                  | Some hours -> (
                      match inter_int_sets p1.minutes p2.minutes with
                      | None -> None
                      | Some minutes -> (
                          match inter_int_sets p1.seconds p2.seconds with
                          | None -> None
                          | Some seconds -> (
                              match inter_ns_sets p1.ns p2.ns with
                              | None -> None
                              | Some ns ->
                                  Some
                                    {
                                      years;
                                      months;
                                      month_days;
                                      weekdays;
                                      hours;
                                      minutes;
                                      seconds;
                                      ns;
                                    })))))))
