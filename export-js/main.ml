open Js_of_ocaml

let _ =
  Js.export_all
    (object%js
      val always = Timere.always

      val empty = Timere.empty

      method years l =
        Js.to_array l
        |> Array.to_list
        |> Timere.years

      method months l = Timere.months l

      method resolve t =
        let s = ref (CCResult.get_exn @@ Timere.resolve ~search_using_tz:Timere.Time_zone.utc t) in
        fun () ->
          match !s () with
          | Seq.Nil -> Js.null
          | Seq.Cons ((x, y), rest) ->
            s := rest;
            Js.some (
              Js.array [|
                Js.string @@ Timere.Date_time.(to_rfc3339 @@ CCResult.get_exn @@ of_timestamp x);
                Js.string @@ Timere.Date_time.(to_rfc3339 @@ CCResult.get_exn @@ of_timestamp y)
              |]
            )
    end)
