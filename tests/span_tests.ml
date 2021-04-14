open Test_utils

module Qc = struct
  let normalize_is_lossless =
    QCheck.Test.make ~count:100_000 ~name:"normalize_is_lossless"
      QCheck.(pair int64 int)
      (fun (s, ns) ->
         let span = Span.normalize { s; ns } in
         Int64.add (Int64.mul s 1_000_000_000L) (Int64.of_int ns)
         = Int64.add
           (Int64.mul Span.(span.s) 1_000_000_000L)
           (Int64.of_int span.ns))

  let normalize_is_idempotent =
    QCheck.Test.make ~count:100_000 ~name:"normalize_is_idempotent" timestamp
      (fun x -> Span.(equal (normalize x) (normalize @@ normalize x)))

  let add_sub =
    QCheck.Test.make ~count:100_000 ~name:"add_sub"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Span.(equal x (x + y - y)))

  let sub_add =
    QCheck.Test.make ~count:100_000 ~name:"sub_add"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Span.(equal x (x - y + y)))

  let sub_self =
    QCheck.Test.make ~count:100_000 ~name:"sub_self" timestamp (fun x ->
        Span.(equal zero (x - x)))

  let double_neg =
    QCheck.Test.make ~count:100_000 ~name:"double_neg" timestamp (fun x ->
        Span.(equal x (neg @@ neg x)))

  let neg =
    QCheck.Test.make ~count:100_000 ~name:"neg" timestamp (fun x ->
        Span.(equal x (neg (neg x - x) - x)))

  let abs =
    QCheck.Test.make ~count:100_000 ~name:"abs" timestamp (fun x ->
        Span.(equal (abs x) (abs (neg x))))

  let abs_is_non_neg =
    QCheck.Test.make ~count:100_000 ~name:"abs_is_non_neg" timestamp (fun x ->
        Span.(abs x >= zero))

  let add_commutative =
    QCheck.Test.make ~count:100_000 ~name:"add_commutative"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Span.(equal (x + y) (y + x)))

  let add_associative =
    QCheck.Test.make ~count:100_000 ~name:"add_associative"
      QCheck.(triple timestamp timestamp timestamp)
      (fun (x, y, z) -> Span.(equal (x + (y + z)) (x + y + z)))

  let add_identity =
    QCheck.Test.make ~count:100_000 ~name:"add_identity" timestamp (fun x ->
        Span.(equal (x + zero) (zero + x)))

  let neg_then_sub_is_same_as_add_then_neg =
    QCheck.Test.make ~count:100_000 ~name:"neg_then_sub_is_same_as_add_then_neg"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Span.(equal (neg x - y) (neg (x + y))))

  let neg_distributive1 =
    QCheck.Test.make ~count:100_000 ~name:"neg_distributive1"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Span.(equal (neg (x + y)) (neg x + neg y)))

  let neg_distributive2 =
    QCheck.Test.make ~count:100_000 ~name:"neg_distributive1"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) -> Span.(equal (neg (x - y)) (neg x - neg y)))

  let add_neg_self =
    QCheck.Test.make ~count:100_000 ~name:"add_neg_self" timestamp (fun x ->
        Span.(equal zero (x + neg x)))

  let neg_add_self =
    QCheck.Test.make ~count:100_000 ~name:"neg_add_self" timestamp (fun x ->
        Span.(equal zero (neg x + x)))

  let to_of_float_accurate =
    QCheck.Test.make ~count:10 ~name:"to_of_float" timestamp (fun x ->
        let x' = Span.of_float @@ Span.to_float x in
        x'.s = x.s && x'.ns - x.ns < 1_000)

  let suite =
    [
      normalize_is_lossless;
      normalize_is_idempotent;
      add_sub;
      sub_add;
      sub_self;
      double_neg;
      neg;
      abs;
      abs_is_non_neg;
      add_commutative;
      add_associative;
      add_identity;
      neg_then_sub_is_same_as_add_then_neg;
      neg_distributive1;
      neg_distributive2;
      add_neg_self;
      neg_add_self;
      to_of_float_accurate;
    ]
end
