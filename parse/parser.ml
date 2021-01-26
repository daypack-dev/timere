open MParser
open Parser_components

exception Invalid_data of string

let prefix_string_match (choices : (string * 'a) list) (s : string) :
  (string * 'a) list =
  let regexp = Re.Str.regexp_case_fold s in
  choices
  |> List.filter (fun (k, _) ->
      try Re.Str.search_forward regexp k 0 = 0 with Not_found -> false)

let invalid_data s = raise (Invalid_data s)

type guess =
  | Dot
  | Comma
  | Hyphen
  | Colon
  | Star
  | Not
  | Outside
  | For
  | Of
  | In
  | To
  | From
  | Am
  | Pm
  | St
  | Nd
  | Rd
  | Th
  | Days
  | Hours
  | Minutes
  | Seconds
  | Nat of int
  | Nats of int Timere.range list
  | Hms of Timere.hms
  | Hmss of Timere.hms Timere.range list
  | Weekday of Timere.weekday
  | Weekdays of Timere.weekday Timere.range list
  | Month_day of int
  | Month_days of int Timere.range list
  | Month of Timere.month
  | Months of Timere.month Timere.range list
  | Duration of Timere.Duration.t

type token = (int * int * int) * guess

type binary_op =
  | Union
  | Inter

type ast =
  | Tokens of token list
  | Binary_op of binary_op * ast * ast
  | Round_robin_pick of ast list

let string_of_token (_, guess) =
  match guess with
  | Dot -> "DOT"
  | Comma -> "COMMA"
  | Hyphen -> "HYPHEN"
  | Colon -> "COLON"
  | Star -> "STAR"
  | Not -> "NOT"
  | Outside -> "outside"
  | For -> "for"
  | Of -> "of"
  | In -> "in"
  | To -> "to"
  | From -> "from"
  | Am -> "am"
  | Pm -> "pm"
  | St -> "st"
  | Nd -> "nd"
  | Rd -> "rd"
  | Th -> "th"
  | Days -> "days"
  | Hours -> "hours"
  | Minutes -> "minutes"
  | Seconds -> "seconds"
  | Nat n -> string_of_int n
  | Nats _ -> "nats"
  | Hms _ -> "hms"
  | Hmss _ -> "hmss"
  | Weekday _ -> "weekday"
  | Weekdays _ -> "weekdays"
  | Month_day _ -> "month_day"
  | Month_days _ -> "month_days"
  | Month _ -> "month"
  | Months _ -> "months"
  | Duration _ -> "duration"

let weekdays : (string * Timere.weekday) list =
  [
    ("sunday", `Sun);
    ("monday", `Mon);
    ("tuesday", `Tue);
    ("wednesday", `Wed);
    ("thursday", `Thu);
    ("friday", `Fri);
    ("saturday", `Sat);
  ]

let months : (string * Timere.month) list =
  [
    ("january", `Jan);
    ("february", `Feb);
    ("march", `Mar);
    ("april", `Apr);
    ("may", `May);
    ("june", `Jun);
    ("july", `Jul);
    ("august", `Aug);
    ("september", `Sep);
    ("october", `Oct);
    ("november", `Nov);
    ("december", `Dec);
  ]

let parse_weekday (s : string) : (Timere.weekday, unit) CCResult.t =
  match prefix_string_match weekdays s with [ (_, x) ] -> Ok x | _ -> Error ()

let parse_month (s : string) : (Timere.month, unit) CCResult.t =
  match prefix_string_match months s with [ (_, x) ] -> Ok x | _ -> Error ()

let weekday_p : (Timere.weekday, unit) t =
  alpha_string
  >>= fun x ->
  if String.length x < 3 then fail (Printf.sprintf "String too short")
  else
    match parse_weekday x with
    | Ok x -> return x
    | Error _ -> fail (Printf.sprintf "Failed to interpret weekday string")

let month_p : (Timere.month, unit) t =
  alpha_string
  >>= fun x ->
  if String.length x < 3 then fail (Printf.sprintf "String too short")
  else
    match parse_month x with
    | Ok x -> return x
    | Error _ -> fail (Printf.sprintf "Failed to interpret month string: %s" x)

let symbols = "()[]&|>"

let token_p : (token, unit) MParser.t =
  get_pos
  >>= fun pos ->
  choice
    [
      attempt (char '.') >>$ Dot;
      attempt (char ',') >>$ Comma;
      attempt (char '-') >>$ Hyphen;
      attempt (char ':') >>$ Colon;
      attempt (char '*') >>$ Star;
      (attempt nat_zero |>> fun x -> Nat x);
      (attempt weekday_p |>> fun x -> Weekday x);
      (attempt month_p |>> fun x -> Month x);
      attempt (string "not") >>$ Not;
      attempt (string "outside") >>$ Outside;
      attempt (string "for") >>$ For;
      attempt (string "of") >>$ Of;
      attempt (string "in") >>$ In;
      attempt (string "to") >>$ To;
      attempt (string "from") >>$ From;
      attempt (string "am") >>$ Am;
      attempt (string "AM") >>$ Am;
      attempt (string "pm") >>$ Pm;
      attempt (string "PM") >>$ Pm;
      attempt (string "st") >>$ St;
      attempt (string "nd") >>$ Nd;
      attempt (string "rd") >>$ Rd;
      attempt (string "th") >>$ Th;
      attempt (string "hours") >>$ Hours;
      attempt (string "hour") >>$ Hours;
      attempt (string "h") >>$ Hours;
      attempt (string "minutes") >>$ Minutes;
      attempt (string "minute") >>$ Minutes;
      attempt (string "mins") >>$ Minutes;
      attempt (string "min") >>$ Minutes;
      attempt (string "m") >>$ Minutes;
      attempt (string "seconds") >>$ Seconds;
      attempt (string "second") >>$ Seconds;
      attempt (string "secs") >>$ Seconds;
      attempt (string "sec") >>$ Seconds;
      attempt (string "s") >>$ Seconds;
      (attempt
         (many1_satisfy (fun c -> c <> ' ' && not (String.contains symbols c)))
       >>= fun s ->
       fail (Printf.sprintf "%s: Unrecognized token: %s" (string_of_pos pos) s));
    ]
  >>= fun guess -> spaces >> return (pos, guess)

let tokens_p = spaces >> many1 token_p << spaces

let inter : (ast -> ast -> ast, unit) t =
  string "&&" >> return (fun a b -> Binary_op (Inter, a, b))

let union : (ast -> ast -> ast, unit) t =
  string "||" >> return (fun a b -> Binary_op (Union, a, b))

let round_robin_pick : (ast -> ast -> ast, unit) t =
  string ">>" >> return (fun a b -> Round_robin_pick [ a; b ])

let expr =
  let rec expr mparser_state =
    let inter_part =
      attempt (char '(')
      >> (spaces >> expr << spaces << char ')')
         <|> (tokens_p |>> fun l -> Tokens l)
    in
    (* let ordered_select_part = chain_left1 inter_part round_robin_pick in *)
    let union_part = chain_left1 inter_part inter in
    chain_left1 union_part union mparser_state
  in
  expr

module Ast_normalize = struct
  let group (type a) ~(extract_single : guess -> a option)
      ~(extract_grouped : guess -> a Timere.range list option)
      ~(constr_grouped : a Timere.range list -> guess) (l : token list) :
    token list =
    let rec recognize_single_interval first_run tokens : token list =
      match tokens with
      | [ (pos_x, x) ] -> (
          match extract_single x with
          | Some x when not first_run ->
            (pos_x, constr_grouped [ `Range_inc (x, x) ])
            :: recognize_single_interval false []
          | _ -> recognize_fallback tokens)
      | (pos_x, x) :: (pos_comma, Comma) :: rest -> (
          match extract_single x with
          | Some x ->
            (pos_x, constr_grouped [ `Range_inc (x, x) ])
            :: (pos_comma, Comma)
            :: recognize_single_interval false rest
          | _ -> recognize_fallback tokens)
      | (pos_x, x) :: (_, To) :: (_, y) :: (pos_comma, Comma) :: rest -> (
          match (extract_single x, extract_single y) with
          | Some x, Some y ->
            (pos_x, constr_grouped [ `Range_inc (x, y) ])
            :: (pos_comma, Comma)
            :: recognize_single_interval false rest
          | _, _ -> recognize_fallback tokens)
      | (pos_x, x) :: (_, To) :: (_, y) :: rest -> (
          match (extract_single x, extract_single y) with
          | Some x, Some y ->
            (pos_x, constr_grouped [ `Range_inc (x, y) ])
            :: recognize_single_interval false rest
          | _, _ -> recognize_fallback tokens)
      | _ -> recognize_fallback tokens
    and recognize_fallback l =
      match l with
      | [] -> []
      | token :: rest -> token :: recognize_single_interval false rest
    in
    let rec merge_intervals tokens : token list =
      match tokens with
      | (pos_x, x) :: (_, Comma) :: (_, y) :: rest -> (
          match (extract_grouped x, extract_grouped y) with
          | Some l1, Some l2 ->
            merge_intervals ((pos_x, constr_grouped (l1 @ l2)) :: rest)
          | _, _ -> merge_fallback tokens)
      | _ -> merge_fallback tokens
    and merge_fallback l =
      match l with [] -> [] | token :: rest -> token :: merge_intervals rest
    in
    l |> recognize_single_interval true |> merge_intervals

  let group_nats (l : token list) : token list =
    group
      ~extract_single:(function Nat x -> Some x | _ -> None)
      ~extract_grouped:(function Nats l -> Some l | _ -> None)
      ~constr_grouped:(fun l -> Nats l)
      l

  let group_months (l : token list) : token list =
    group
      ~extract_single:(function Month x -> Some x | _ -> None)
      ~extract_grouped:(function Months l -> Some l | _ -> None)
      ~constr_grouped:(fun x -> Months x)
      l

  let group_weekdays (l : token list) : token list =
    group
      ~extract_single:(function Weekday x -> Some x | _ -> None)
      ~extract_grouped:(function Weekdays l -> Some l | _ -> None)
      ~constr_grouped:(fun x -> Weekdays x)
      l

  let recognize_month_day (l : token list) : token list =
    let rec recognize_single tokens =
      match tokens with
      | (pos_x, Nat x) :: (_, St) :: rest
      | (pos_x, Nat x) :: (_, Nd) :: rest
      | (pos_x, Nat x) :: (_, Rd) :: rest
      | (pos_x, Nat x) :: (_, Th) :: rest ->
        (pos_x, Month_day x) :: recognize_single rest
      | [] -> []
      | x :: xs -> x :: recognize_single xs
    in
    let rec propagate_guesses tokens =
      match tokens with
      | (pos_x, Month_day x) :: (pos_comma, Comma) :: (pos_y, Nat y) :: rest ->
        (pos_x, Month_day x)
        :: (pos_comma, Comma)
        :: propagate_guesses ((pos_y, Month_day y) :: rest)
      | (pos_x, Month_day x) :: (pos_to, To) :: (pos_y, Nat y) :: rest ->
        (pos_x, Month_day x)
        :: (pos_to, To)
        :: propagate_guesses ((pos_y, Month_day y) :: rest)
      | [] -> []
      | x :: xs -> x :: propagate_guesses xs
    in
    l
    |> recognize_single
    |> propagate_guesses
    |> List.rev
    |> propagate_guesses
    |> List.rev

  let group_month_days (l : token list) : token list =
    group
      ~extract_single:(function Month_day x -> Some x | _ -> None)
      ~extract_grouped:(function Month_days l -> Some l | _ -> None)
      ~constr_grouped:(fun x -> Month_days x)
      l

  type hms_mode =
    | Hms_24
    | Hms_am
    | Hms_pm

  let recognize_hms (l : token list) : token list =
    let make_hms mode ~pos_hour ~hour ?pos_minute ?(minute = 0) ?pos_second
        ?(second = 0) () : token =
      let hour =
        match mode with
        | Hms_24 ->
          if 0 <= hour && hour < 24 then hour
          else
            invalid_data
              (Printf.sprintf "%s: Invalid hour: %d" (string_of_pos pos_hour)
                 hour)
        | Hms_am ->
          if 1 <= hour && hour <= 12 then hour mod 12
          else
            invalid_data
              (Printf.sprintf "%s: Invalid hour: %d am"
                 (string_of_pos pos_hour) hour)
        | Hms_pm ->
          if 1 <= hour && hour <= 12 then (hour mod 12) + 12
          else
            invalid_data
              (Printf.sprintf "%s: Invalid hour: %d pm"
                 (string_of_pos pos_hour) hour)
      in
      if 0 <= minute && minute < 60 then
        if 0 <= second && second < 60 then
          (pos_hour, Hms (Timere.make_hms_exn ~hour ~minute ~second))
        else
          invalid_data
            (Printf.sprintf "%s: Invalid second: %d"
               (string_of_pos @@ CCOpt.get_exn @@ pos_second)
               minute)
      else
        invalid_data
          (Printf.sprintf "%s: Invalid minute: %d"
             (string_of_pos @@ CCOpt.get_exn @@ pos_minute)
             minute)
    in
    let rec aux acc (l : token list) : token list =
      match l with
      | (pos_hour, Nat hour)
        :: (_, Colon)
        :: (pos_minute, Nat minute)
        :: (_, Colon) :: (pos_second, Nat second) :: (_, Am) :: rest ->
        let token =
          make_hms Hms_am ~pos_hour ~hour ~pos_minute ~minute ~pos_second
            ~second ()
        in
        aux (token :: acc) rest
      | (pos_hour, Nat hour)
        :: (_, Colon)
        :: (pos_minute, Nat minute)
        :: (_, Colon) :: (pos_second, Nat second) :: (_, Pm) :: rest ->
        let token =
          make_hms Hms_pm ~pos_hour ~hour ~pos_minute ~minute ~pos_second
            ~second ()
        in
        aux (token :: acc) rest
      | (pos_hour, Nat hour)
        :: (_, Colon)
        :: (pos_minute, Nat minute)
        :: (_, Colon) :: (pos_second, Nat second) :: rest ->
        let token =
          make_hms Hms_24 ~pos_hour ~hour ~pos_minute ~minute ~pos_second
            ~second ()
        in
        aux (token :: acc) rest
      | (pos_hour, Nat hour)
        :: (_, Colon) :: (pos_minute, Nat minute) :: (_, Am) :: rest ->
        let token = make_hms Hms_am ~pos_hour ~hour ~pos_minute ~minute () in
        aux (token :: acc) rest
      | (pos_hour, Nat hour)
        :: (_, Colon) :: (pos_minute, Nat minute) :: (_, Pm) :: rest ->
        let token = make_hms Hms_pm ~pos_hour ~hour ~pos_minute ~minute () in
        aux (token :: acc) rest
      | (pos_hour, Nat hour) :: (_, Colon) :: (pos_minute, Nat minute) :: rest
        ->
        let token = make_hms Hms_24 ~pos_hour ~hour ~pos_minute ~minute () in
        aux (token :: acc) rest
      | (pos_hour, Nat hour) :: (_, Am) :: rest ->
        let token = make_hms Hms_am ~pos_hour ~hour () in
        aux (token :: acc) rest
      | (pos_hour, Nat hour) :: (_, Pm) :: rest ->
        let token = make_hms Hms_pm ~pos_hour ~hour () in
        aux (token :: acc) rest
      | [] -> List.rev acc
      | token :: rest -> aux (token :: acc) rest
    in
    aux [] l

  let group_hms (l : token list) : token list =
    group
      ~extract_single:(function Hms x -> Some x | _ -> None)
      ~extract_grouped:(function Hmss l -> Some l | _ -> None)
      ~constr_grouped:(fun x -> Hmss x)
      l

  let recognize_duration (l : token list) : token list =
    let make_duration ~pos ~days ~hours ~minutes ~seconds =
      ( CCOpt.get_exn pos,
        Duration
          (Timere.Duration.make
             ~days:(CCOpt.value ~default:0 days)
             ~hours:(CCOpt.value ~default:0 hours)
             ~minutes:(CCOpt.value ~default:0 minutes)
             ~seconds ()) )
    in
    let rec aux_start_with_days acc l =
      match l with
      | (pos, Nat days) :: (_, Days) :: rest ->
        aux_start_with_hours ~pos:(Some pos) ~days:(Some days) acc rest
      | _ -> aux_start_with_hours ~pos:None ~days:None acc l
    and aux_start_with_hours ~pos ~days acc l =
      match l with
      | (pos_hours, Nat hours) :: (_, Hours) :: rest ->
        aux_start_with_minutes
          ~pos:(Some (CCOpt.value ~default:pos_hours pos))
          ~days ~hours:(Some hours) acc rest
      | _ -> aux_start_with_minutes ~pos ~days ~hours:None acc l
    and aux_start_with_minutes ~pos ~days ~hours acc l =
      match l with
      | (pos_minutes, Nat minutes) :: (_, Minutes) :: rest ->
        aux_start_with_seconds
          ~pos:(Some (CCOpt.value ~default:pos_minutes pos))
          ~days ~hours ~minutes:(Some minutes) acc rest
      | _ -> aux_start_with_seconds ~pos ~days ~hours ~minutes:None acc l
    and aux_start_with_seconds ~pos ~days ~hours ~minutes acc l =
      match l with
      | (pos_seconds, Nat seconds) :: (_, Seconds) :: rest ->
        let token =
          ( CCOpt.value ~default:pos_seconds pos,
            Duration
              (Timere.Duration.make
                 ~days:(CCOpt.value ~default:0 days)
                 ~hours:(CCOpt.value ~default:0 hours)
                 ~minutes:(CCOpt.value ~default:0 minutes)
                 ~seconds ()) )
        in
        aux_start_with_days (token :: acc) rest
      | [] ->
        if CCOpt.is_some days || CCOpt.is_some hours || CCOpt.is_some minutes
        then
          let new_token =
            make_duration ~pos ~days ~hours ~minutes ~seconds:0
          in
          List.rev (new_token :: acc)
        else List.rev acc
      | token :: rest ->
        if CCOpt.is_some days || CCOpt.is_some hours || CCOpt.is_some minutes
        then
          let new_token =
            make_duration ~pos ~days ~hours ~minutes ~seconds:0
          in
          aux_start_with_days (token :: new_token :: acc) rest
        else aux_start_with_days (token :: acc) rest
    in
    aux_start_with_days [] l

  let process_tokens (e : ast) : (ast, string) CCResult.t =
    let rec aux e =
      match e with
      | Tokens l ->
        let l =
          l |> recognize_hms |> recognize_duration |> recognize_month_day
          (* |> group_nats
           * |> group_month_days
           * |> group_weekdays
           * |> group_months
           * |> group_hms *)
        in
        Tokens l
      | Binary_op (op, e1, e2) -> Binary_op (op, aux e1, aux e2)
      | Round_robin_pick l -> Round_robin_pick (List.map aux l)
    in
    try Ok (aux e) with Invalid_data msg -> Error msg

  let flatten_round_robin_select (e : ast) : ast =
    let rec aux e =
      match e with
      | Tokens _ -> e
      | Binary_op (op, e1, e2) -> Binary_op (op, aux e1, aux e2)
      | Round_robin_pick l ->
        l
        |> CCList.to_seq
        |> Seq.map aux
        |> Seq.flat_map (fun e ->
            match e with
            | Round_robin_pick l -> CCList.to_seq l
            | _ -> Seq.return e)
        |> CCList.of_seq
        |> fun l -> Round_robin_pick l
    in
    aux e

  let normalize (e : ast) : (ast, string) CCResult.t =
    e |> flatten_round_robin_select |> process_tokens
end

let parse_into_ast (s : string) : (ast, string) CCResult.t =
  parse_string
    (expr
     << spaces
     >>= fun e ->
     get_pos
     >>= fun pos ->
     attempt eof
     >> return e
        <|> fail (Printf.sprintf "Expected EOI, pos: %s" (string_of_pos pos)))
    s ()
  |> result_of_mparser_result

let flatten_months pos (l : Timere.month Timere.range list) =
  match Timere.Utils.flatten_month_range_list l with
  | Some x -> Ok x
  | None ->
    Error
      (Some (Printf.sprintf "%s: Invalid month ranges" (string_of_pos pos)))

let flatten_weekdays pos (l : Timere.weekday Timere.range list) =
  match Timere.Utils.flatten_weekday_range_list l with
  | Some x -> Ok x
  | None ->
    Error
      (Some (Printf.sprintf "%s: Invalid weekday ranges" (string_of_pos pos)))

let flatten_month_days pos (l : int Timere.range list) =
  match Timere.Utils.flatten_month_day_range_list l with
  | Some x -> Ok x
  | None ->
    Error
      (Some
         (Printf.sprintf "%s: Invalid month day ranges" (string_of_pos pos)))

let pattern ?(years = []) ?(months = []) ?pos_days ?(days = []) ?(weekdays = [])
    ?(hms : Timere.hms option) () =
  if not (List.for_all (fun x -> 1 <= x && x <= 31) days) then
    Error
      (Some
         (Printf.sprintf "%s: Invalid month days"
            (string_of_pos @@ CCOpt.get_exn @@ pos_days)))
  else
    match hms with
    | None -> Ok (Timere.pattern ~years ~months ~days ~weekdays ())
    | Some hms ->
      Ok
        (Timere.pattern ~years ~months ~days ~weekdays ~hours:[ hms.hour ]
           ~minutes:[ hms.minute ] ~seconds:[ hms.second ] ())

let t_rules : (token list -> (Timere.t, string option) CCResult.t) list =
  [
    (function [ (_, Star) ] -> Ok Timere.always | _ -> Error None);
    (function
      | [ (_, Weekday x) ] -> Ok (Timere.weekdays [ x ])
      | [ (pos, Weekdays l) ] ->
        flatten_weekdays pos l |> CCResult.map (fun l -> Timere.weekdays l)
      | _ -> Error None);
    (function
      | [ (_, Month_day x) ] -> Ok (Timere.days [ x ])
      | [ (pos, Month_days l) ] ->
        flatten_month_days pos l |> CCResult.map (fun l -> Timere.days l)
      | _ -> Error None);
    (function
      | [ (_, Month x) ] -> Ok (Timere.months [ x ])
      | [ (pos, Months l) ] ->
        flatten_months pos l |> CCResult.map (fun l -> Timere.months l)
      | _ -> Error None);
    (function
      | [ (_, Nat year); (_, Month month); (pos_days, Nat day) ]
      | [ (_, Nat year); (_, Month month); (pos_days, Nat day); (_, St) ]
      | [ (_, Nat year); (_, Month month); (pos_days, Nat day); (_, Nd) ]
      | [ (_, Nat year); (_, Month month); (pos_days, Nat day); (_, Rd) ]
      | [ (_, Nat year); (_, Month month); (pos_days, Nat day); (_, Th) ]
        when year > 31 ->
        pattern ~years:[ year ] ~months:[ month ] ~pos_days ~days:[ day ] ()
      | [ (pos_days, Nat day); (_, Month month); (_, Nat year) ] when year > 31 ->
        pattern ~years:[ year ] ~months:[ month ] ~pos_days ~days:[ day ] ()
      | [ (_, Nat year); (pos_days, Nat day); (_, Of); (_, Month month) ] ->
        pattern ~years:[ year ] ~months:[ month ] ~pos_days ~days:[ day ] ()
      | _ -> Error None);
    (function
      | [ (_, Month month); (pos_days, Nat day); (_, Hms hms) ] ->
        pattern ~months:[ month ] ~pos_days ~days:[ day ] ~hms ()
      | _ -> Error None);
    (function
      | [ (_, Nat year); (_, Month month); (pos_days, Nat day); (_, Hms hms) ] ->
        pattern ~years:[ year ] ~months:[ month ] ~pos_days ~days:[ day ] ~hms
          ()
      | _ -> Error None);
  ]

let t_of_tokens (tokens : token list) : (Timere.t, string) CCResult.t =
  let rec aux tokens rules =
    match rules with
    | [] ->
      let pos, _ = List.hd tokens in
      (* List.iter
       *   (fun token -> print_endline (string_of_token token))
       *   tokens; *)
      Error
        (Printf.sprintf "%s: Unrecognized token pattern" (string_of_pos pos))
    | rule :: rest -> (
        match rule tokens with
        | Ok time -> Ok time
        | Error None -> aux tokens rest
        | Error (Some msg) -> Error msg)
  in
  aux tokens t_rules

let t_of_ast (ast : ast) : (Timere.t, string) CCResult.t =
  let rec aux ast =
    match ast with
    | Tokens tokens -> t_of_tokens tokens
    | Binary_op (op, ast1, ast2) -> (
        match aux ast1 with
        | Error msg -> Error msg
        | Ok time1 -> (
            match aux ast2 with
            | Error msg -> Error msg
            | Ok time2 -> (
                match op with
                | Union -> Ok (Timere.union [ time1; time2 ])
                | Inter -> Ok (Timere.inter [ time1; time2 ]))))
    | Round_robin_pick l -> (
        match l |> List.map aux |> Misc_utils.get_ok_error_list with
        | Error msg -> Error msg
        | Ok _l ->
          (* Ok (Timere.round_robin_pick l) *)
          failwith "Unimplemented")
  in
  aux ast

let parse_timere s =
  match parse_into_ast s with
  | Error msg -> Error msg
  | Ok ast -> (
      match Ast_normalize.normalize ast with
      (* match Ok ast with *)
      | Error msg -> Error msg
      | Ok ast -> t_of_ast ast)

let date_time_t_of_ast ~tz (ast : ast) : (Timere.Date_time.t, string) CCResult.t
  =
  match ast with
  | Tokens [ (_, Nat year); (_, Month month); (_, Nat day); (_, Hms hms) ]
    when year > 31 -> (
      match
        Timere.Date_time.make ~year ~month ~day ~hour:hms.hour
          ~minute:hms.minute ~second:hms.second ~tz
      with
      | Some x -> Ok x
      | None -> Error "Invalid date time")
  | Tokens [ (_, Nat day); (_, Month month); (_, Nat year); (_, Hms hms) ]
    when year > 31 -> (
      match
        Timere.Date_time.make ~year ~month ~day ~hour:hms.hour
          ~minute:hms.minute ~second:hms.second ~tz
      with
      | Some x -> Ok x
      | None -> Error "Invalid date time")
  | Tokens
      [ (_, Nat year); (_, Month month); (_, Nat day); (_, St); (_, Hms hms) ]
  | Tokens
      [ (_, Nat year); (_, Month month); (_, Nat day); (_, Nd); (_, Hms hms) ]
  | Tokens
      [ (_, Nat year); (_, Month month); (_, Nat day); (_, Rd); (_, Hms hms) ]
  | Tokens
      [ (_, Nat year); (_, Month month); (_, Nat day); (_, Th); (_, Hms hms) ]
    -> (
        match
          Timere.Date_time.make ~year ~month ~day ~hour:hms.hour
            ~minute:hms.minute ~second:hms.second ~tz
        with
        | Some x -> Ok x
        | None -> Error "Invalid date time")
  | _ -> Error "Unrecognized pattern"

let parse_date_time ?(tz = Timere.Time_zone.utc) s =
  match parse_into_ast s with
  | Error msg -> Error msg
  | Ok ast -> (
      match Ast_normalize.normalize ast with
      | Error msg -> Error msg
      | Ok ast -> date_time_t_of_ast ~tz ast)

let duration_t_of_ast (ast : ast) : (Timere.Duration.t, string) CCResult.t =
  match ast with
  | Tokens [ (_, Duration duration) ] -> Ok duration
  | _ -> Error "Unrecognized pattern"

let parse_duration s =
  match parse_into_ast s with
  | Error msg -> Error msg
  | Ok ast -> (
      match Ast_normalize.normalize ast with
      | Error msg -> Error msg
      | Ok ast -> duration_t_of_ast ast)
