open Test_utils

module Qc = struct
  let to_of_span =
    QCheck.Test.make ~count:100_000 ~name:"to_of_span" duration (fun duration ->
        Duration.equal duration (Duration.of_span @@ Duration.to_span duration))

  let of_to_span =
    QCheck.Test.make ~count:100_000 ~name:"of_to_span" timestamp (fun span ->
        Span.equal span (Duration.to_span @@ Duration.of_span span))

  let normalize_is_idempotent =
    QCheck.Test.make ~count:10_000 ~name:"normalize_is_idempotent" duration
      (fun x -> Duration.(equal (normalize x) (normalize @@ normalize x)))

  let suite = [ to_of_span; of_to_span; normalize_is_idempotent ]
end
