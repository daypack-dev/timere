open Test_utils

module Qc = struct
  let to_of_iso_week_date =
    QCheck.Test.make ~count:100_000 ~name:"to_of_iso_week_date" iso_ord_date
      (fun d ->
         let d' =
           d
           |> Timedesc.Date.ISO_ord_date.to_iso_week_date
           |> Timedesc.Date.ISO_ord_date.of_iso_week_date
         in
         Timedesc.Date.ISO_ord_date.equal d d')

  let of_to_iso_week_date =
    QCheck.Test.make ~count:100_000 ~name:"of_to_iso_week_date" iso_week_date
      (fun d ->
         let d' =
           d
           |> Timedesc.Date.ISO_ord_date.of_iso_week_date
           |> Timedesc.Date.ISO_ord_date.to_iso_week_date
         in
         Timedesc.Date.ISO_week_date.equal d d')

  let to_of_ymd_date =
    QCheck.Test.make ~count:100_000 ~name:"to_of_ymd_date" iso_ord_date
      (fun d ->
         let d' =
           d
           |> Timedesc.Date.ISO_ord_date.to_ymd_date
           |> Timedesc.Date.ISO_ord_date.of_ymd_date
         in
         Timedesc.Date.ISO_ord_date.equal d d')

  let of_to_ymd_date =
    QCheck.Test.make ~count:100_000 ~name:"of_to_ymd_date" ymd_date (fun d ->
        let d' =
          d
          |> Timedesc.Date.ISO_ord_date.of_ymd_date
          |> Timedesc.Date.ISO_ord_date.to_ymd_date
        in
        Timedesc.Date.Ymd_date.equal d d')

  let suite =
    [ to_of_iso_week_date; of_to_iso_week_date; to_of_ymd_date; of_to_ymd_date ]
end
