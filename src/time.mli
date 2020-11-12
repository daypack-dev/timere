type tz_offset_s = int

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

type month =
  [ `Jan
  | `Feb
  | `Mar
  | `Apr
  | `May
  | `Jun
  | `Jul
  | `Aug
  | `Sep
  | `Oct
  | `Nov
  | `Dec
  ]

type t

val chunk : int64 -> t -> t

val inter : t -> t -> t

val union : t -> t -> t

val not_in : t -> t

val interval_inc : t -> t -> t

val interval_exc : t -> t -> t

val flatten : t Seq.t -> t

val flatten_list : t list -> t

val of_pattern :
  ?years : int list ->
  ?months : month list ->
  ?month_days : int list ->
  ?weekdays : weekday list ->
  ?hours : int list ->
  ?minutes : int list ->
  ?seconds : int list ->
  ?unix_seconds : int64 list ->
  unit ->
  t
