val timere : string -> (Timere.t, string) result

val date_time :
  ?tz:Timere.Time_zone.t -> string -> (Timere.Date_time.t, string) result

val hms : string -> (Timere.hms, string) result

val duration : string -> (Timere.Duration.t, string) result
