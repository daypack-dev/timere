type unary_op =
  | Not
  | Shift of Timedesc.Span.t
  | Lengthen of Timedesc.Span.t
  | With_tz of Timedesc.Time_zone.t

type chunking =
  [ `Disjoint_intervals
  | `By_duration of Timedesc.Span.t
  | `By_duration_drop_partial of Timedesc.Span.t
  | `At_year_boundary
  | `At_month_boundary
  ]

type chunked_unary_op_on_t =
  | Chunk_disjoint_interval
  | Chunk_at_year_boundary
  | Chunk_at_month_boundary
  | Chunk_by_duration of {
      chunk_size : Timedesc.Span.t;
      drop_partial : bool;
    }

type chunked_unary_op_on_chunked =
  | Drop of int
  | Take of int
  | Take_nth of int
  | Nth of int
  | Chunk_again of chunked_unary_op_on_t

type t =
  | Empty
  | All
  | Intervals of (Timedesc.Span.t * Timedesc.Span.t) Seq.t
  | Pattern of Pattern.t
  | Unary_op of unary_op * t
  | Inter_seq of t Seq.t
  | Union_seq of t Seq.t
  | Pattern_intervals of {
      mode : [ `Whole_inc | `Whole_exc | `Fst | `Snd ];
      bound : Timedesc.Span.t;
      start : Points.t;
      end_ : Points.t;
    }
  | Unchunk of chunked

and chunked =
  | Unary_op_on_t of chunked_unary_op_on_t * t
  | Unary_op_on_chunked of chunked_unary_op_on_chunked * chunked
