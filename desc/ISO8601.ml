let date_p : (Date.t, unit) MParser.t =
  let open MParser in
  let open Parser_components in
  nat_zero
  >>= fun year ->
  char '-'
  >> max_two_digit_nat_zero
  >>= fun month ->
  char '-'
  >> max_two_digit_nat_zero
  >>= fun day ->
  match Date.Ymd_date.make ~year ~month ~day with
  | Ok x -> return x
  | Error e ->
      fail
        (Printf.sprintf "Invalid date: %s"
           (Date_time.Ymd_date_time.string_of_error
              (e :> Date_time.Ymd_date_time.error)))

let hm_p : (int * int, unit) MParser.t =
  let open MParser in
  let open Parser_components in
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

let time_p : (Time.t, unit) MParser.t =
  let open MParser in
  let open Parser_components in
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
  hms_p
  >>= fun (hour, minute, second, ns) ->
  match Time.make ~hour ~minute ~second ~ns () with
  | Ok x -> return x
  | Error e ->
      fail
        (Printf.sprintf "Invalid time: %s"
           (Date_time.Ymd_date_time.string_of_error
              (e :> Date_time.Ymd_date_time.error)))

let offset_p : (Span.t, unit) MParser.t =
  let open MParser in
  char 'Z'
  >>$ Span.zero
  <|> (char '+'
      >>$ `Pos
      <|> (char '-' >>$ `Neg)
      >>= fun sign ->
      hm_p
      |>> fun (hour, minute) ->
      Span.For_human'.make_exn ~sign ~hours:hour ~minutes:minute ())

type maybe_zoneless =
  [ `Zoned of Date_time.t
  | `Zoneless of Date_time.Zoneless'.zoneless
  ]

let to_maybe_zoneless s : (maybe_zoneless, string) result =
  let open MParser in
  let open Parser_components in
  let p =
    date_p
    >>= fun date ->
    any_char
    >> time_p
    >>= fun time ->
    attempt (offset_p << spaces << eof)
    >>= (fun offset ->
          match
            Date_time.Zoneless'.to_zoned_unambiguous ~offset_from_utc:offset
              (Date_time.Zoneless'.make date time)
          with
          | Error e ->
              fail
                (Printf.sprintf "Invalid date time: %s"
                   (Date_time.Ymd_date_time.string_of_error
                      (e :> Date_time.Ymd_date_time.error)))
          | Ok x -> return (`Zoned x))
    <|> (spaces
        >> eof
        >> return (`Zoneless (Date_time.Zoneless'.make date time)))
  in
  parse_string p s () |> result_of_mparser_result

let to_zoneless s : (Date_time.Zoneless'.zoneless, string) result =
  match to_maybe_zoneless s with
  | Ok (`Zoneless x) -> Ok x
  | Ok (`Zoned _) -> Error "Extraneous offset from utc"
  | Error msg -> Error msg

let to_date_time s : (Date_time.t, string) result =
  match to_maybe_zoneless s with
  | Ok (`Zoneless _) -> Error "Missing offset from utc"
  | Ok (`Zoned x) -> Ok x
  | Error msg -> Error msg

let to_timestamp s =
  match to_date_time s with
  | Ok dt -> Ok (Date_time.to_timestamp_single dt)
  | Error msg -> Error msg
