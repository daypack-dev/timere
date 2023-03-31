open Test_utils

module Alco = struct
  let suite = []
end

module Qc = struct
  let jd_of_ymd_then_ymd_of_jd =
    QCheck.Test.make ~count:100_000 ~name:"jd_of_ymd_then_ymd_of_jd"
      ymd_date
      (fun (year, month, day) ->
         let jd = Timedesc.Utils.jd_of_ymd ~year ~month ~day in
         let year', month', day' = Timedesc.Utils.ymd_of_jd jd in
         year = year' && month = month' && day = day'
      )

  let jd_of_iso_week_date_then_iso_week_date_of_jd =
    QCheck.Test.make ~count:100_000 ~name:"jd_of_iso_week_date_then_iso_week_date_of_jd"
      iso_week_date
      (fun (year, week, weekday) ->
         let jd = Timedesc.Utils.jd_of_iso_week_date ~year ~week ~weekday in
         let year', week', weekday' = Timedesc.Utils.iso_week_date_of_jd jd in
         year = year' && week = week' && weekday = weekday'
      )

  let suite =
    [
      jd_of_ymd_then_ymd_of_jd;
      jd_of_iso_week_date_then_iso_week_date_of_jd;
    ]
end
