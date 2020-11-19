val timere : string -> (Timere.t, string) result

val date_time :
  ?tz_offset_s:Timere.tz_offset_s -> string -> (Timere.Date_time.t, string) result

val duration : string -> (Timere.Duration.t, string) result
