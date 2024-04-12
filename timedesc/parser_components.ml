open Angstrom

let is_letter c =
  match c with
  | 'A'..'Z' -> true
  | 'a'..'z' -> true
  | _ -> false

let is_space c =
  match c with
  | ' ' -> true
  | '\t' -> true
  | '\n' -> true
  | _ -> false

let is_not_space c =
  not (is_space c)

let is_digit c =
  match c with
  | '0'..'9' -> true
  | _ -> false

let digit : char t =
  satisfy is_digit

let alpha_string : string t =
  take_while1 is_letter

let any_string : string t = take_while (fun _ -> true)

let take_space : string t = take_while is_space

let optional_char target =
  peek_char
  >>= (fun c ->
      match c with
      | None -> return ()
      | Some c ->
        if c = target then
          any_char *> return ()
        else
          return ()
    )

let ident_string ~(reserved_words : string list) : string t =
  let reserved_words = List.map String.lowercase_ascii reserved_words in
  alpha_string
  >>= fun s ->
  if List.mem (String.lowercase_ascii s) reserved_words then
    fail (Printf.sprintf "\"%s\" is a reserved word" s)
  else return s

let skip_non_num_string ~end_markers =
  skip_while (function
      | '0' .. '9' -> false
      | c -> (
          match end_markers with
          | None -> true
          | Some x -> not (String.contains x c)))

let num_string : string t =
  take_while1 (function '0' .. '9' -> true | _ -> false)

let nat_zero : int t =
  num_string
  >>= fun s ->
  try return (int_of_string s)
  with _ -> fail (Printf.sprintf "integer %s is out of range" s)

let nat_zero_w_original_str : (int * string) t =
  num_string
  >>= fun s ->
  try return (int_of_string s, s)
  with _ -> fail (Printf.sprintf "integer %s is out of range" s)

let one_digit_nat_zero : int t =
  digit >>= fun c -> return (int_of_string (Printf.sprintf "%c" c))

let two_digit_nat_zero : int t =
  digit
  >>= fun c1 ->
  digit
  >>= fun c2 -> return (int_of_string (Printf.sprintf "%c%c" c1 c2))

let max_two_digit_nat_zero : int t =
  two_digit_nat_zero
  <|> (digit >>= fun c -> return (int_of_string (Printf.sprintf "%c" c)))

let float_non_neg : float t =
  take_while1 is_digit
  >>= fun x ->
  char '.'
  *> take_while1 is_digit
  >>= fun y ->
  let s = x ^ "." ^ y in
  try return (float_of_string s)
  with _ -> fail (Printf.sprintf "float %s is out of range" s)

let comma : char t = char ','

let dot : char t = char '.'

let hyphen : char t = char '-'

let non_square_bracket_string : string t =
  take_while (function '[' | ']' -> false | _ -> true)

let non_curly_bracket_string : string t =
  take_while (function '{' | '}' -> false | _ -> true)

let non_parenthesis_string : string t =
  take_while (function '(' | ')' -> false | _ -> true)

let non_space_string : string t = take_while1 is_not_space

let spaces = skip_while is_space

let spaces1 = take_while1 is_space *> return ()

let sep_by_comma (p : 'a t) : 'a list t =
  sep_by (spaces *> comma *> spaces) p

let sep_by_comma1 (p : 'a t) : 'a list t =
  sep_by1 (spaces *> comma *> spaces) p

(* let sep_res_seq ~by ~end_markers (p : ('a, 'b) t) : ('a result Seq.t, 'b) t =
 *   sep_by (char by)
 *     ( get_pos
 *       >>= fun pos ->
 *       many1_satisfy (fun c -> c <> by && not (String.contains end_markers c))
 *       >>= fun s -> return (pos, s) )
 *   |>> fun l ->
 *     ( l
 *       |> CCList.to_seq
 *       |> Seq.map (fun (pos, s) ->
 *           match
 *           parse_string
 *             ( p
 *               >>= fun x ->
 *               attempt eoi >> return x
 *               <|> ( get_pos
 *                     >>= fun pos ->
 *                     any_string
 *                     >>= fun s -> (fail (Printf.sprintf "invalid syntax: %s, pos: %s" s (string_of_pos pos) )) )
 *             )
 *             s
 *             ()
 *           with
 *           | Success 
 *     )
 * 
 * let sep_res ~by ~end_markers (p : 'a t) : ('a, string) result list t =
 *   sep_res_seq ~by ~end_markers p >>= fun s -> return (CCList.of_seq s)
 * 
 * let sep_fail_on_first_fail ~by ~end_markers (p : 'a t) : 'a list t =
 *   sep_res_seq ~by ~end_markers p
 *   >>= fun s ->
 *   match Seq_utils.find_first_error_or_return_whole_list s with
 *   | Ok l -> return l
 *   | Error s -> fail s *)

(* let chainl1 x op =
 *   let rec aux a =
 *     attempt (spaces >> op << spaces)
 *     >>= (fun f -> x >>= fun b -> aux (f a b))
 *         <|> return a
 *   in
 *   x >>= aux *)

let invalid_syntax ~text ~pos =
  fail (Printf.sprintf "invalid syntax: %s, pos: %d" text pos)

let extraneous_text_check ~end_markers =
  spaces
  *> pos
  >>= fun pos ->
  take_while (fun c -> not (String.contains end_markers c))
  >>= fun s ->
  match s with "" -> return () | text -> invalid_syntax ~text ~pos

(*let result_of_mparser_result (x : 'a result) : ('a, string) Stdlib.result =
  match x with
  | Success x -> Ok x
  | Failed (_, err) -> (
      match err with
      | No_error -> Error "unknown error"
      | Parse_error (pos, msgs) -> (
          match
            List.fold_left
              (fun res msg ->
                 match res with
                 | Some x -> Some x
                 | None -> (
                     match msg with
                     | Unexpected_error s ->
                       Some
                         (Printf.sprintf "unexpected: %s, pos: %s" s
                            (string_of_pos pos))
                     | Expected_error s ->
                       Some
                         (Printf.sprintf "expected: %s, pos: %s" s
                            (string_of_pos pos))
                     | Message_error s -> Some s
                     | Compound_error (s, _) -> Some s
                     | Backtrack_error _ -> res
                     | Unknown_error -> res))
              None msgs
          with
          | None ->
            Error
              (Printf.sprintf "unknown error, pos: %s" (string_of_pos pos))
          | Some s -> Error s))
*)
