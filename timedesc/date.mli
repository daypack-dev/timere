open Date_time_utils

type t = private { jd : int }

val equal : t -> t -> bool

val lt : t -> t -> bool

val le : t -> t -> bool

val gt : t -> t -> bool

val ge : t -> t -> bool

val compare : t -> t -> int

val add : days:int -> t -> t

val sub : days:int -> t -> t

val diff_days : t -> t -> int

val year : t -> int

val month : t -> int

val day : t -> int

val weekday : t -> weekday

val ym : t -> Ym.t

val iso_year : t -> int

val iso_week : t -> ISO_week.t

val day_of_year : t -> int

module ISO_week_date' : sig
  type view = private {
    year : int;
    week : int;
    weekday : weekday;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_iso_year of int
    | `Invalid_iso_week of int
    ]

  exception Error_exn of error

  val make : year:int -> week:int -> weekday:weekday -> (t, error) result

  val make_exn : year:int -> week:int -> weekday:weekday -> t

  val view : t -> view
end

val of_iso_week : ISO_week.t -> weekday:weekday -> t

module Ymd' : sig
  type view = private {
    year : int;
    month : int;
    day : int;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_month of int
    | `Invalid_day of int
    ]

  exception Error_exn of error

  val make : year:int -> month:int -> day:int -> (t, error) result

  val make_exn : year:int -> month:int -> day:int -> t

  val view : t -> view
end

val of_ym : Ym.t -> day:int -> (t, Ymd'.error) result

val of_ym_exn : Ym.t -> day:int -> t

module ISO_ord' : sig
  type view = private {
    year : int;
    day_of_year : int;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_day_of_year of int
    ]

  exception Error_exn of error

  val make : year:int -> day_of_year:int -> (t, error) result

  val make_exn : year:int -> day_of_year:int -> t

  val view : t -> view
end
