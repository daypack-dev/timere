open Date_time_utils

type t = private { jd : int }

val equal : t -> t -> bool

val year : t -> int

val month : t -> int

val day : t -> int

val weekday : t -> weekday

val iso_week_year : t -> int

val iso_week : t -> int

val day_of_year : t -> int

module ISO_week_date : sig
  type view = private {
    iso_week_year : int;
    iso_week : int;
    weekday : weekday;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_iso_week_year of int
    | `Invalid_iso_week of int
    ]

  exception Error_exn of error

  val make :
    iso_week_year:int -> iso_week:int -> weekday:weekday -> (t, error) result

  val make_exn : iso_week_year:int -> iso_week:int -> weekday:weekday -> t

  val view : t -> view
end

module Ymd_date : sig
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

module ISO_ord_date : sig
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
