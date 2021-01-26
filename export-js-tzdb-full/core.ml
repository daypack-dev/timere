open Js_of_ocaml

let list_of_js_array arr = arr |> Js.to_array |> Array.to_list

let js_array_of_list l = l |> Array.of_list |> Js.array

let raise_with_msg msg =
  Js.raise_js_error (new%js Js.error_constr (Js.string msg))

let wrap f = try f () with Invalid_argument msg -> raise_with_msg msg

let js_date_of_date_time dt =
  let open Timere.Date_time in
  let date = new%js Js.date_now in
  let _ = date##setUTCFullYear dt.year in
  let _ = date##setUTCMonth (Timere.Utils.tm_int_of_month dt.month) in
  let _ = date##setUTCDate dt.day in
  let _ = date##setUTCHours dt.hour in
  let _ = date##setUTCMinutes dt.minute in
  let _ = date##setUTCSeconds dt.second in
  let _ = date##setUTCMilliseconds 0 in
  date

let js_date_of_timestamp x =
  match Timere.Date_time.of_timestamp x with
  | None -> raise_with_msg "Invalid timestamp"
  | Some dt -> js_date_of_date_time dt

let month_of_int x =
  match Timere.Utils.month_of_tm_int x with
  | None -> raise_with_msg "Invalid month"
  | Some x -> x

let weekday_of_int x =
  match Timere.Utils.weekday_of_tm_int x with
  | None -> raise_with_msg "Invalid weekday"
  | Some x -> x

let date_time_of_js_date (date : Js.date Js.t) =
  let year = date##getUTCFullYear in
  let month = month_of_int date##getUTCMonth in
  let day = date##getUTCDate in
  let hour = date##getUTCHours in
  let minute = date##getUTCMinutes in
  let second = date##getUTCSeconds in
  match
    Timere.Date_time.make ~year ~month ~day ~hour ~minute ~second
      ~tz:Timere.Time_zone.utc
  with
  | None -> raise_with_msg "Invalid date"
  | Some x -> x

let to_be_exported =
  object%js
    val always = Timere.always

    val empty = Timere.empty

    method years l = wrap (fun () -> Timere.years (list_of_js_array l))

    method months l =
      wrap (fun () ->
          list_of_js_array l
          |> List.map (fun x ->
              match Timere.Utils.month_of_human_int x with
              | Some x -> x
              | None -> raise_with_msg "Invalid month int")
          |> Timere.months)

    method days l = wrap (fun () -> Timere.days (list_of_js_array l))

    method weekdays l =
      wrap (fun () ->
          list_of_js_array l |> List.map weekday_of_int |> Timere.weekdays)

    method hours l = wrap (fun () -> Timere.hours (list_of_js_array l))

    method minutes l = wrap (fun () -> Timere.minutes (list_of_js_array l))

    method seconds l = wrap (fun () -> Timere.seconds (list_of_js_array l))

    method nthWeekdayOfMonth n weekday =
      Timere.nth_weekday_of_month n (weekday_of_int weekday)

    method inter l = wrap (fun () -> Timere.inter (list_of_js_array l))

    method union l = wrap (fun () -> Timere.union (list_of_js_array l))

    method not x = wrap (fun () -> Timere.not x)

    val duration =
      object%js
        method d days = wrap (fun () -> Timere.Duration.make_frac ~days ())

        method dh days hours =
          wrap (fun () -> Timere.Duration.make_frac ~days ~hours ())

        method dhm days hours minutes =
          wrap (fun () -> Timere.Duration.make_frac ~days ~hours ~minutes ())

        method dhms days hours minutes seconds =
          wrap (fun () ->
              Timere.Duration.make_frac ~days ~hours ~minutes ~seconds ())

        method days x = Timere.Duration.(x.days)

        method hours x = Timere.Duration.(x.hours)

        method minutes x = Timere.Duration.(x.minutes)

        method seconds x = Timere.Duration.(x.seconds)
      end

    method shift dur x = wrap (fun () -> Timere.shift dur x)

    method lengthen dur x = wrap (fun () -> Timere.lengthen dur x)

    val timeZone =
      object%js
        method make name = wrap (fun () -> Timere.Time_zone.make_exn name)

        method name t = Timere.Time_zone.name t

        val utc = Timere.Time_zone.utc

        method equal t1 t2 = Js.bool @@ Timere.Time_zone.equal t1 t2

        val available_time_zones =
          js_array_of_list Timere.Time_zone.available_time_zones

        method makeOffsetOnly offset = Timere.Time_zone.make_offset_only offset

        method toJSONString t =
          Yojson.Basic.to_string @@ Timere.Time_zone.JSON.to_json t

        method ofJSONString s =
          match Timere.Time_zone.JSON.of_string (Js.to_string s) with
          | None -> raise_with_msg "Invalid JSON string"
          | Some x -> x
      end

    method withTZ tz t = wrap (fun () -> Timere.with_tz tz t)

    method date date = Timere.date_time (date_time_of_js_date date)

    method dates dates =
      dates
      |> list_of_js_array
      |> List.map date_time_of_js_date
      |> Timere.date_times

    val points =
      object%js
        method s second = Timere.make_points ~second ()

        method ms minute second = Timere.make_points ~minute ~second ()

        method hms hour minute second =
          Timere.make_points ~hour ~minute ~second ()

        method whms weekday hour minute second =
          Timere.make_points ~weekday:(weekday_of_int weekday) ~hour ~minute
            ~second ()

        method dhms day hour minute second =
          Timere.make_points ~day ~hour ~minute ~second ()

        method mdhms month day hour minute second =
          Timere.make_points ~month:(month_of_int month) ~day ~hour ~minute
            ~second ()
      end

    method resolve t =
      match Timere.resolve ~search_using_tz:Timere.Time_zone.utc t with
      | Error msg -> raise_with_msg msg
      | Ok s ->
        let s = ref s in
        fun () ->
          wrap (fun () ->
              match !s () with
              | Seq.Nil -> Js.null
              | Seq.Cons ((x, y), rest) ->
                s := rest;
                Js.some
                  (Js.array
                     [| js_date_of_timestamp x; js_date_of_timestamp y |]))

    method to_sexp_string t = Js.string (Timere.to_sexp_string t)

    method of_sexp_string s =
      match Timere.of_sexp_string (Js.to_string s) with
      | Error msg -> raise_with_msg msg
      | Ok x -> x
  end
