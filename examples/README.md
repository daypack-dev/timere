# Timere examples

## Simple

`simple.ml` is the one shown in repo README

## Date time

`date_time.ml` demonstrates basic date time handling with time zone

## Querying across DST boundary

(See `dst.ml` for full runnable example)

```
  with_tz (Timere.Time_zone.make_exn "Australia/Sydney")
    (
      years [2020] (* in year 2020 *)
      &
      (pattern ~months:[`Apr] ~month_day_ranges:[`Range_inc (2, 7)] () (* in April 2 to 7 *)
       ||| pattern ~months:[`Oct] ~month_day_ranges:[`Range_inc (1, 6)] ()) (* or in Oct 1 to 6 *)
      &
      hms_interval_exc (* 11pm to 3am *)
        (make_hms_exn ~hour:23 ~minute:0 ~second:0)
        (make_hms_exn ~hour:3 ~minute:0 ~second:0)
    )
```

gives (with manual annotation `<- ...`)

```
[2020 Apr 02 00:00:00 +11:00:00, 2020 Apr 02 03:00:00 +11:00:00) - 3 hours 0 mins 0 secs
[2020 Apr 02 23:00:00 +11:00:00, 2020 Apr 03 03:00:00 +11:00:00) - 4 hours 0 mins 0 secs
[2020 Apr 03 23:00:00 +11:00:00, 2020 Apr 04 03:00:00 +11:00:00) - 4 hours 0 mins 0 secs
[2020 Apr 04 23:00:00 +11:00:00, 2020 Apr 05 03:00:00 +10:00:00) - 5 hours 0 mins 0 secs <- DST ends at Apr 5th 2am
[2020 Apr 05 23:00:00 +10:00:00, 2020 Apr 06 03:00:00 +10:00:00) - 4 hours 0 mins 0 secs
[2020 Apr 06 23:00:00 +10:00:00, 2020 Apr 07 03:00:00 +10:00:00) - 4 hours 0 mins 0 secs
[2020 Apr 07 23:00:00 +10:00:00, 2020 Apr 08 00:00:00 +10:00:00) - 1 hours 0 mins 0 secs
[2020 Oct 01 00:00:00 +10:00:00, 2020 Oct 01 03:00:00 +10:00:00) - 3 hours 0 mins 0 secs
[2020 Oct 01 23:00:00 +10:00:00, 2020 Oct 02 03:00:00 +10:00:00) - 4 hours 0 mins 0 secs
[2020 Oct 02 23:00:00 +10:00:00, 2020 Oct 03 03:00:00 +10:00:00) - 4 hours 0 mins 0 secs
[2020 Oct 03 23:00:00 +10:00:00, 2020 Oct 04 03:00:00 +11:00:00) - 3 hours 0 mins 0 secs <- DST starts at Oct 4th 2am
[2020 Oct 04 23:00:00 +11:00:00, 2020 Oct 05 03:00:00 +11:00:00) - 4 hours 0 mins 0 secs
[2020 Oct 05 23:00:00 +11:00:00, 2020 Oct 06 03:00:00 +11:00:00) - 4 hours 0 mins 0 secs
[2020 Oct 06 23:00:00 +11:00:00, 2020 Oct 07 00:00:00 +11:00:00) - 1 hours 0 mins 0 secs
```
