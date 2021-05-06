open Date_time_components

module ISO_week_date : sig
  type t = private {
    iso_week_year : int;
    week : int;
    weekday : weekday;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_iso_week_year of int
    | `Invalid_week of int
    ]

  exception Error_exn of error

  val equal : t -> t -> bool

  val make :
    iso_week_year:int -> week:int -> weekday:weekday -> (t, error) result

  val make_exn : iso_week_year:int -> week:int -> weekday:weekday -> t
end

module Ymd_date : sig
  type t = private {
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

  val equal : t -> t -> bool

  val make : year:int -> month:int -> day:int -> (t, error) result

  val make_exn : year:int -> month:int -> day:int -> t
end

module ISO_ord_date : sig
  type t = private {
    year : int;
    day_of_year : int;
  }

  type error =
    [ `Does_not_exist
    | `Invalid_year of int
    | `Invalid_day_of_year of int
    ]

  exception Error_exn of error

  val equal : t -> t -> bool

  val make : year:int -> day_of_year:int -> (t, error) result

  val make_exn : year:int -> day_of_year:int -> t

  val weekday : t -> weekday

  val to_iso_week_date : t -> ISO_week_date.t

  val of_iso_week_date : ISO_week_date.t -> t

  val to_ymd_date : t -> Ymd_date.t

  val of_ymd_date : Ymd_date.t -> t
end
