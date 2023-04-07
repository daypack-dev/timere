(* The use of word "date" here follows RFC9110, even though
   it does not match the use of date elsewhere in the library.

   The difference is hidden in the final interface, however.
*)

let gmt_p : unit Angstrom.t =
  let open Angstrom in
  string_ci "GMT" *> return ()

let weekday_p : unit Angstrom.t =
  let open Angstrom in
  let open Parser_components in
  alpha_string *> return ()

let month_p : int Angstrom.t =
  let open Angstrom in
  let open Parser_components in
  alpha_string >>= fun s ->
  if String.length s < 3 then
    fail (Printf.sprintf "Invalid month: %s" s)
  else (
    (match String.lowercase_ascii @@ StringLabels.sub ~pos:0 ~len:3 s with
     | "jan" -> return 1
     | "feb" -> return 2
     | "mar" -> return 3
     | "apr" -> return 4
     | "may" -> return 5
     | "jun" -> return 6
     | "jul" -> return 7
     | "aug" -> return 8
     | "sep" -> return 9
     | "oct" -> return 10
     | "nov" -> return 11
     | "dec" -> return 12
     | _ -> fail (Printf.sprintf "Invalid month: %s" s)
    )
  )

let hms_p =
  let open Angstrom in
  let open Parser_components in
  two_digit_nat_zero
  >>= fun hour ->
  char ':'
  *> two_digit_nat_zero
  >>= fun minute ->
  char ':'
  *> two_digit_nat_zero
  >>= fun second ->
  return (hour, minute, second)

let time_p : Time.t Angstrom.t =
  let open Angstrom in
  hms_p
  >>= fun (hour, minute, second) ->
  match Time.make ~hour ~minute ~second () with
  | Ok x -> return x
  | Error e ->
    fail
      (Printf.sprintf "Invalid time: %s"
         (Date_time.Ymd_date_time.string_of_error
            (e :> Date_time.Ymd_date_time.error)))

let date' ~year ~month ~day : Date.t Angstrom.t =
  let open Angstrom in
  match Date.Ymd'.make ~year ~month ~day with
  | Ok x -> return x
  | Error e ->
    fail
      (Printf.sprintf "Invalid date: %s"
         (Date_time.Ymd_date_time.string_of_error
            (e :> Date_time.Ymd_date_time.error)))

let date_time' date time =
  let open Angstrom in
  match
    Date_time.Ymd_date_time.of_date_and_time_unambiguous ~offset_from_utc:Span.zero
      date time
  with
  | Error e ->
    fail
      (Printf.sprintf "Invalid date time: %s"
         (Date_time.Ymd_date_time.string_of_error
            (e :> Date_time.Ymd_date_time.error)))
  | Ok x -> return x

let imf_fixdate_p =
  let open Angstrom in
  let open Parser_components in
  weekday_p *> spaces *> comma *> spaces
  *> max_two_digit_nat_zero
  >>= fun day ->
  spaces *> month_p >>= fun month ->
  spaces *> nat_zero >>= fun year ->
  date' ~year ~month ~day >>= fun date ->
  spaces *> time_p >>= fun time ->
  spaces *> gmt_p *>
  date_time' date time

let rfc850_date_p =
  let open Angstrom in
  let open Parser_components in
  weekday_p *> spaces *> comma *> spaces
  *> max_two_digit_nat_zero >>= fun day ->
  spaces *> char '-' *> spaces *> month_p >>= fun month ->
  spaces *> char '-' *> spaces
  *> max_two_digit_nat_zero >>= fun year ->
  let year =
    if year >= 50 then
      year + 1900
    else
      year + 2000
  in
  spaces *> date' ~year ~month ~day >>= fun date ->
  time_p >>= fun time ->
  spaces *> gmt_p *>
  date_time' date time

let asctime_date_p =
  let open Angstrom in
  let open Parser_components in
  weekday_p *> spaces *>
  month_p >>= fun month ->
  spaces *> max_two_digit_nat_zero >>= fun day ->
  spaces *> time_p >>= fun time ->
  spaces *> nat_zero >>= fun year ->
  date' ~year ~month ~day >>= fun date ->
  date_time' date time

let date_time_of_str s : (Date_time.t, string) result =
  let open Angstrom in
  let open Parser_components in
  let p =
    choice
      [
        imf_fixdate_p;
        rfc850_date_p;
        asctime_date_p;
      ]
  in
  parse_string ~consume:All (p <* spaces) s

let timestamp_of_str s =
  match date_time_of_str s with
  | Ok dt -> Ok (Date_time.to_timestamp_single dt)
  | Error msg -> Error msg
