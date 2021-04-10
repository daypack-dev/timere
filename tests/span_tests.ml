open Test_utils

module Qc = struct
  let normalize_is_idempotent =
    QCheck.Test.make ~count:10_000 ~name:"normalize_is_idempotent"
      timestamp
      (fun x ->
         Span.(equal (normalize x) (normalize @@ normalize x))
      )

  let add_sub =
    QCheck.Test.make ~count:10_000 ~name:"add_sub"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) ->
         Span.(equal x (x + y - y))
      )

  let sub_add =
    QCheck.Test.make ~count:10_000 ~name:"sub_add"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) ->
         Span.(equal x (x - y + y))
      )

  let sub_self =
    QCheck.Test.make ~count:10_000 ~name:"sub_self"
      timestamp
      (fun x ->
         Span.(equal zero (x - x))
      )

  let double_neg =
    QCheck.Test.make ~count:10_000 ~name:"double_neg"
      timestamp
      (fun x ->
         Span.(equal x (neg @@ neg x))
      )

  let neg =
    QCheck.Test.make ~count:10_000 ~name:"neg"
      timestamp
      (fun x ->
         Span.(equal x (neg (neg x - x) - x))
      )

  let abs =
    QCheck.Test.make ~count:10_000 ~name:"abs"
      timestamp
      (fun x ->
         Span.(equal (abs x) (abs (neg x)))
      )

  let add_commutative =
    QCheck.Test.make ~count:10_000 ~name:"add_commutative"
      QCheck.(pair timestamp timestamp)
      (fun (x, y) ->
         Span.(equal (x + y) (y + x))
      )

  let suite = [normalize_is_idempotent;
               add_sub;
               sub_add;
               sub_self;
               double_neg;
               neg;
               abs;
               add_commutative;
              ]
end
