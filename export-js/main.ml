open Js_of_ocaml

let js_date_of_timestamp x =
  let open Timere.Date_time in
  let dt = CCResult.get_exn @@ of_timestamp x in
  let date = new%js Js.date_now in
  (* let date = new%js Js.date_sec dt.year (Timere.Utils.tm_int_of_month dt.month) dt.day dt.hour dt.minute dt.second in *)
  let _ = date##setUTCFullYear dt.year in
  let _ = date##setUTCMonth (Timere.Utils.tm_int_of_month dt.month) in
  let _ = date##setUTCDate dt.day in
  let _ = date##setUTCHours dt.hour in
  let _ = date##setUTCMinutes dt.minute in
  let _ = date##setUTCSeconds dt.second in
  let _ = date##setUTCMilliseconds 0 in
  date

let _ =
  Js.export_all
    (object%js
      val always = Timere.always

      val empty = Timere.empty

      method years l = Js.to_array l |> Array.to_list |> Timere.years

      method months l = Timere.months l

      method resolve t =
        let s =
          ref
            (CCResult.get_exn
             @@ Timere.resolve ~search_using_tz:Timere.Time_zone.utc t)
        in
        fun () ->
          match !s () with
          | Seq.Nil -> Js.null
          | Seq.Cons ((x, y), rest) ->
            s := rest;
            Js.some
              (Js.array [| js_date_of_timestamp x; js_date_of_timestamp y |])
    end)
