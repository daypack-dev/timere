open MParser
open Parser_components

type guess =
  | Dot
  | Comma
  | Hyphen
  | Colon
  | Star
  | To
  | From
  | Am
  | Pm
  | Nat of int
  | Weekday of Time.weekday
  | Month of Time.month

type token = (int * int * int) * guess

type binary_op =
  | Union
  | Inter

type ast =
  | Tokens of token list
  | Binary_op of binary_op * ast * ast
  | Round_robin_pick of ast list

let weekdays : (string * Time.weekday) list =
  [
    ("sunday", `Sun);
    ("monday", `Mon);
    ("tuesday", `Tue);
    ("wednesday", `Wed);
    ("thursday", `Thu);
    ("friday", `Fri);
    ("saturday", `Sat);
  ]

let months : (string * Time.month) list =
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

let parse_weekday (s : string) : (Time.weekday, unit) Result.t =
  match Misc_utils.prefix_string_match weekdays s with
  | [ (_, x) ] -> Ok x
  | _ -> Error ()

let parse_month (s : string) : (Time.month, unit) Result.t =
  match Misc_utils.prefix_string_match months s with
  | [ (_, x) ] -> Ok x
  | _ -> Error ()

let weekday_p : (Time.weekday, unit) t =
  alpha_string
  >>= fun x ->
  match parse_weekday x with
  | Ok x -> return x
  | Error _ -> fail (Printf.sprintf "Failed to interpret weekday string")

let month_p : (Time.month, unit) t =
  alpha_string
  >>= fun x ->
  match parse_month x with
  | Ok x -> return x
  | Error _ -> fail (Printf.sprintf "Failed to interpret month string: %s" x)

(* type static_str_p = (string, unit) MParser.t
 * 
 * let not_str : static_str_p = string "not"
 * 
 * let next_slot_str : static_str_p = string "next-slot"
 * 
 * let next_point_str : static_str_p =
 *   attempt (string "next-point") <|> string "next-pt"
 * 
 * let next_batch_str : static_str_p = string "next-batch"
 * 
 * let next_str : static_str_p = string "next"
 * 
 * let point_str : static_str_p = string "point"
 * 
 * let slot_str : static_str_p = string "slot"
 * 
 * let points_str : static_str_p = string "points"
 * 
 * let slots_str : static_str_p = string "slots"
 * 
 * let batch_str : static_str_p = string "batch"
 * 
 * let batches_str : static_str_p = string "batches"
 * 
 * let of_str : static_str_p = string "of"
 * 
 * let from_str : static_str_p = string "from"
 * 
 * let to_str : static_str_p = string "to"
 * 
 * let first_str : static_str_p = string "first"
 * 
 * let last_str : static_str_p = string "last" *)

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
      attempt (string "to") >>$ To;
      attempt (string "from") >>$ From;
      attempt (string "am") >>$ Am;
      attempt (string "AM") >>$ Am;
      attempt (string "pm") >>$ Pm;
      attempt (string "PM") >>$ Pm;
      (attempt nat_zero |>> fun x -> Nat x);
      (attempt weekday_p |>> fun x -> Weekday x);
      (attempt month_p |>> fun x -> Month x);
      ( non_space_string
        >>= fun s ->
        fail (Printf.sprintf "%s - Unrecognized token: %s" (string_of_pos pos) s)
      );
    ]
  >>= fun guess -> spaces >>$ (pos, guess)

let tokens_p = spaces >> sep_by token_p spaces1 << spaces

let inter : (ast -> ast -> ast, unit) t =
  string "&&" >> return (fun a b -> Binary_op (Inter, a, b))

let union : (ast -> ast -> ast, unit) t =
  string "||" >> return (fun a b -> Binary_op (Union, a, b))

let round_robin_pick : (ast -> ast -> ast, unit) t =
  string ">>" >> return (fun a b -> Round_robin_pick [ a; b ])

let flatten_round_robin_select (e : ast) : ast =
  let rec aux e =
    match e with
    | Tokens _ -> e
    | Binary_op (op, e1, e2) -> Binary_op (op, aux e1, aux e2)
    | Round_robin_pick l ->
      l
      |> List.to_seq
      |> Seq.map aux
      |> Seq.flat_map (fun e ->
          match e with
          | Round_robin_pick l -> List.to_seq l
          | _ -> Seq.return e)
      |> List.of_seq
      |> fun l -> Round_robin_pick l
  in
  aux e

let expr =
  let rec expr mparser_state =
    let inter_part =
      attempt (char '(')
      >> (spaces >> expr << spaces << char ')')
         <|> (tokens_p |>> fun l -> Tokens l)
    in
    let ordered_select_part = chain_left1 inter_part round_robin_pick in
    let union_part = chain_left1 ordered_select_part inter in
    chain_left1 union_part union mparser_state
  in
  expr >>= fun e -> return (flatten_round_robin_select e)

let parse_into_ast (s : string) : (ast, string) Result.t =
  parse_string
    ( expr
      << spaces
      >>= fun e ->
      get_pos
      >>= fun pos ->
      attempt eof
      >> return e
         <|> fail (Printf.sprintf "Expected EOI, pos: %s" (string_of_pos pos)) )
    s ()
  |> result_of_mparser_result

let rules : (token list -> (Time.t, unit) Result.t) list =
  let open Time in
  [
    (fun l -> match l with [ (_, Star) ] -> Ok Time.any | _ -> Error ());
    (fun l ->
       match l with
       | [ (_, Nat year); (_, Month month); (_, Nat day) ] ->
         Ok (pattern ~years:[ year ] ~months:[ month ] ~month_days:[ day ] ())
       | _ -> Error ());
  ]

let time_t_of_tokens (tokens : token list) : (Time.t, string) Result.t =
  let rec aux tokens rules =
    match rules with
    | [] -> Error "Unrecognized text pattern"
    | rule :: rest -> (
        match rule tokens with
        | Ok time -> Ok time
        | Error () -> aux tokens rest )
  in
  aux tokens rules

let time_t_of_ast (ast : ast) : (Time.t, string) Result.t =
  let rec aux ast =
    match ast with
    | Tokens tokens -> time_t_of_tokens tokens
    | Binary_op (op, ast1, ast2) -> (
        match aux ast1 with
        | Error msg -> Error msg
        | Ok time1 -> (
            match aux ast2 with
            | Error msg -> Error msg
            | Ok time2 -> (
                match op with
                | Union -> Ok (Time.union time1 time2)
                | Inter -> Ok (Time.inter time1 time2) ) ) )
    | Round_robin_pick l -> (
        match l |> List.map aux |> Misc_utils.get_ok_error_list with
        | Error msg -> Error msg
        | Ok l -> Ok (Time.round_robin_pick l) )
  in
  aux ast

let parse s =
  match parse_into_ast s with
  | Error msg -> Error msg
  | Ok ast -> time_t_of_ast ast
