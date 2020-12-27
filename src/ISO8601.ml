let of_date_time (dt : Time.Date_time.t) : string =
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d%s" dt.year (Time.human_int_of_month dt.month)
    dt.day dt.hour dt.minute dt.second
    (match dt.tz_info with
     | `Tz_only _ -> invalid_arg "Date time has no exact offset"
     | `Tz_offset_s_only x | `Tz_and_tz_offset_s (_, x) ->
       let sign =
         if x < 0 then '-' else '+'
       in
       let offset =
         Result.get_ok @@ Duration.of_seconds (Int64.of_int (abs x))
       in
       Printf.sprintf "%c%02d:%02d" sign offset.hours offset.minutes
    )

let pp_date_time formatter dt =
  Format.fprintf formatter "%s" (of_date_time dt)

let of_timestamp (x : int64) : string =
  match Time.Date_time.of_timestamp x with
  | Error () -> invalid_arg "Invalid timestamp"
  | Ok dt ->
    of_date_time dt

let pp_timestamp formatter x =
  Format.fprintf formatter "%s" (of_timestamp x)

let to_date_time s : (Time.Date_time.t, string) result =
  let open MParser in
  let open Parser_components in
  let two_digit_nat_zero =
    pair digit digit >>= fun (c1, c2) ->
    return (int_of_string (Printf.sprintf "%c%c" c1 c2))
  in
  let hm_p =
    choice
      [
        attempt (two_digit_nat_zero >>= fun hour ->
                 optional (char ':') >> two_digit_nat_zero >>= fun minute ->
                 return (hour, minute)
                );
        (two_digit_nat_zero >>= fun hour ->
         return (hour, 0)
        );
      ]
  in
  let hms_p =
    choice
      [
        attempt (two_digit_nat_zero >>= fun hour ->
                 optional (char ':') >> two_digit_nat_zero >>= fun minute ->
                 optional (char ':') >> two_digit_nat_zero >>= fun second ->
                 return (hour, minute, second)
                );
        hm_p |>> fun (hour, minute) -> (hour, minute, 0)
      ]
  in
  let offset_p =
    (char 'Z' >>$ 0)
    <|>
    (
      ((char '+' >>$ (1)) <|> (char '-' >>$ (-1))) >>= fun mult ->
      hm_p |>> fun (hour, minute) ->
      (
        Duration.make ~hours:hour ~minutes:minute ()
        |> Result.get_ok
        |> Duration.to_seconds
        |> Int64.to_int
        |> Int.mul mult
      )
    )
  in
  let p =
    nat_zero >>= fun year ->
    char '-' >> two_digit_nat_zero >>= fun month ->
    match Time.month_of_human_int month with
    | Error () -> fail "Invalid month"
    | Ok month ->
      char '-' >> two_digit_nat_zero >>= fun day ->
      optional (char 'T') >> hms_p >>= fun (hour, minute, second) ->
      offset_p >>= fun offset ->
      match Time.Date_time.make_precise ~year ~month ~day ~hour ~minute ~second ~tz_offset_s:offset () with
      | Error () -> fail "Invalid date time"
      | Ok x -> return x
  in
  parse_string p s ()
  |> result_of_mparser_result
