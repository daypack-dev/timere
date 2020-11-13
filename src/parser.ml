open MParser
open Parser_components

type ambiguous =
  | Years of int list
  | Months of Time.month list
  | Month_days of int list
  | Weekdays of Time.weekday list
  | Hours of int list
  | Minutes of int list
  | Seconds of int list

type guess =
  | Uncertain of ambiguous list
  | Dot
  | Comma
  | To
  | From

type token = {
  pos : int * int * int;
  guess : guess;
}

type binary_op =
  | Union
  | Inter

type ast =
  | Tokens of token list
  | Binary_op of binary_op * ast * ast
  | Round_robin_pick of ast list

type static_str_p = (string, unit) MParser.t

let not_str : static_str_p = string "not"

let next_slot_str : static_str_p = string "next-slot"

let next_point_str : static_str_p =
  attempt (string "next-point") <|> string "next-pt"

let next_batch_str : static_str_p = string "next-batch"

let next_str : static_str_p = string "next"

let point_str : static_str_p = string "point"

let slot_str : static_str_p = string "slot"

let points_str : static_str_p = string "points"

let slots_str : static_str_p = string "slots"

let batch_str : static_str_p = string "batch"

let batches_str : static_str_p = string "batches"

let of_str : static_str_p = string "of"

let from_str : static_str_p = string "from"

let to_str : static_str_p = string "to"

let first_str : static_str_p = string "first"

let last_str : static_str_p = string "last"

let token_p : (token, unit) MParser.t =
  get_pos
  >>= fun pos ->
  choice
    [
      attempt (char '.') >>$ Dot;
      attempt (char ',') >>$ Comma;
      attempt to_str >>$ To;
      attempt from_str >>$ From;
    ]
  |>> fun guess -> { pos; guess }

let tokens_p = many1 token_p

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

let of_string (s : string) : (ast, string) Result.t =
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
