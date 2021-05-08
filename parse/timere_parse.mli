val timere : string -> (Timere.t, string) result

val date_time :
  ?tz:Timedesc.Time_zone.t -> string -> (Timedesc.t, string) result

val hms : string -> (Timere.Hms.t, string) result

val duration : string -> (Timedesc.Span.t, string) result
