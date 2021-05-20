exception Error_exn of string

val timere : string -> (Timere.t, string) result

val timere_exn : string -> Timere.t

val date_time :
  ?tz:Timedesc.Time_zone.t -> string -> (Timedesc.t, string) result

val date_time_exn : ?tz:Timedesc.Time_zone.t -> string -> Timedesc.t

val hms : string -> (Timere.Hms.t, string) result

val hms_exn : string -> Timere.Hms.t

val duration : string -> (Timedesc.Span.t, string) result

val duration_exn : string -> Timedesc.Span.t
