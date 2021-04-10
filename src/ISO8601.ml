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
           >>= fun second -> return (hour, minute, second));
        (hm_p |>> fun (hour, minute) -> (hour, minute, 0));
      ]
  in
  let offset_p =
    char 'Z'
    >>$ 0
        <|> (char '+'
             >>$ 1
                 <|> (char '-' >>$ -1)
             >>= fun mult ->
             hm_p
             |>> fun (hour, minute) ->
             Duration.make ~hours:hour ~minutes:minute ()
             |> Duration.to_span
             |> (fun x -> Int64.to_int x.s)
             |> CCInt.mul mult)
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
        >>= fun (hour, minute, second) ->
        offset_p
        >>= fun offset ->
        match
          Time.Date_time'.make_precise ~year ~month ~day ~hour ~minute ~second
            ~tz_offset_s:offset ()
        with
        | None -> fail "Invalid date time"
        | Some x -> return x)
  in
  parse_string p s () |> result_of_mparser_result
