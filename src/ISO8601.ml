open Date_time_components

let to_date_time s : (Time.Date_time'.t, string) result =
  let open MParser in
  let open Parser_components in
  let two_digit_nat_zero =
    pair digit digit
    >>= fun (c1, c2) -> return (int_of_string (Printf.sprintf "%c%c" c1 c2))
  in
  let hm_p =
    choice
      [
        attempt
          (two_digit_nat_zero
           >>= fun hour ->
           optional (char ':')
           >> two_digit_nat_zero
           >>= fun minute -> return (hour, minute));
        (two_digit_nat_zero >>= fun hour -> return (hour, 0));
      ]
  in
  let hms_p =
    choice
      [
        attempt
          (two_digit_nat_zero
           >>= fun hour ->
           optional (char ':')
           >> two_digit_nat_zero
           >>= fun minute ->
           optional (char ':')
           >> two_digit_nat_zero
           >>= fun second ->
           choice [ char '.'; char ',' ]
           >> num_string
           >>= fun s ->
           let s = if String.length s > 9 then String.sub s 0 9 else s in
           let len = String.length s in
           if len = 9 then return (hour, minute, second, int_of_string s)
           else
             let diff = 9 - len in
             let ns =
               int_of_float
               @@ CCFloat.round
               @@ (float_of_int (int_of_string s) *. (10. ** float_of_int diff))
             in
             return (hour, minute, second, ns));
        attempt
          (two_digit_nat_zero
           >>= fun hour ->
           optional (char ':')
           >> two_digit_nat_zero
           >>= fun minute ->
           optional (char ':')
           >> two_digit_nat_zero
           >>= fun second -> return (hour, minute, second, 0));
        (hm_p |>> fun (hour, minute) -> (hour, minute, 0, 0));
      ]
  in
  let offset_p =
    char 'Z'
    >>$ Duration.zero
        <|> (char '+'
             >>$ `Pos
                 <|> (char '-' >>$ `Neg)
             >>= fun sign ->
             hm_p
             |>> fun (hour, minute) ->
             Duration.make ~sign ~hours:hour ~minutes:minute ())
  in
  let p =
    nat_zero
    >>= fun year ->
    char '-'
    >> two_digit_nat_zero
    >>= fun month ->
    match month_of_human_int month with
    | None -> fail "Invalid month"
    | Some month -> (
        char '-'
        >> two_digit_nat_zero
        >>= fun day ->
        optional (char 'T')
        >> hms_p
        >>= fun (hour, minute, second, ns) ->
        offset_p
        >>= fun offset ->
        let hour, minute, second, ns =
          if hour = 24 && minute = 0 && second = 0 && ns = 0 then
            (23, 59, 59, Span.ns_count_in_s - 1)
          else (hour, minute, second, ns)
        in
        match
          Time.Date_time'.make_unambiguous ~year ~month ~day ~hour ~minute
            ~second ~ns ~tz_offset:offset ()
        with
        | None -> fail "Invalid date time"
        | Some x -> return x
        | exception Invalid_argument msg -> fail msg)
  in
  parse_string p s () |> result_of_mparser_result

let to_timestamp s =
  match to_date_time s with
  | Ok dt -> Ok (Time.Date_time'.to_timestamp_single dt)
  | Error msg -> Error msg
