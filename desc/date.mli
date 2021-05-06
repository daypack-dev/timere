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

  val equal : t -> t -> bool

  val make : iso_week_year:int -> week:int -> weekday:weekday -> (t, error) result
end

module Ymd : sig
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

  val equal : t -> t -> bool

  val make : year:int -> month:int -> day:int -> (t, error) result
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

  val equal : t -> t -> bool

  val make : year:int -> day_of_year:int -> (t, error) result

  val weekday : t -> weekday

  val to_iso_week_date : t -> ISO_week_date.t

  val of_iso_week_date : ISO_week_date.t -> t

  val to_ymd : t -> Ymd.t

  val of_ymd : Ymd.t -> t
end
