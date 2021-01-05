open MParser

let alpha_string : (string, unit) t = many1_chars letter

let any_string : (string, unit) t = many_satisfy (fun _ -> true)

let take_space : (string, unit) t = many_chars space

let ident_string ~(reserved_words : string list) : (string, unit) t =
  let reserved_words = List.map String.lowercase_ascii reserved_words in
  alpha_string
  >>= fun s ->
  if List.mem (String.lowercase_ascii s) reserved_words then
    fail (Printf.sprintf "\"%s\" is a reserved word" s)
  else return s

let skip_non_num_string ~end_markers =
  skip_satisfy (function
      | '0' .. '9' -> false
      | c -> (
          match end_markers with
          | None -> true
          | Some x -> not (String.contains x c)))

let nat_zero : (int, unit) t =
  many1_satisfy (function '0' .. '9' -> true | _ -> false)
  >>= fun s ->
  try return (int_of_string s)
  with _ -> fail (Printf.sprintf "Integer %s is out of range" s)

let float_non_neg : (float, unit) t =
  many1_satisfy (function '0' .. '9' -> true | _ -> false)
  >>= fun x ->
  attempt
    (char '.' >> many1_satisfy (function '0' .. '9' -> true | _ -> false))
  <|> return "0"
  >>= fun y ->
  let s = x ^ "." ^ y in
  try return (float_of_string s)
  with _ -> fail (Printf.sprintf "Float %s is out of range" s)

let comma : (char, unit) t = char ','

let dot : (char, unit) t = char '.'

let hyphen : (char, unit) t = char '-'

let non_square_bracket_string : (string, unit) t =
  many_satisfy (function '[' | ']' -> false | _ -> true)

let non_parenthesis_string : (string, unit) t =
  many_satisfy (function '(' | ')' -> false | _ -> true)

let non_space_string : (string, unit) t = many1_chars non_space

let sep_by_comma (p : ('a, unit) t) : ('a list, unit) t =
  sep_by p (attempt (spaces >> comma >> spaces))

let sep_by_comma1 (p : ('a, unit) t) : ('a list, unit) t =
  sep_by1 p (attempt (spaces >> comma >> spaces))

let option (default : 'a) p : ('a, 'b) t = attempt p <|> return default

let string_of_pos pos =
  let _index, lnum, cnum = pos in
  Printf.sprintf "%d:%d" lnum cnum

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
 *                     >>= fun s -> (fail (Printf.sprintf "Invalid syntax: %s, pos: %s" s (string_of_pos pos) )) )
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
  fail (Printf.sprintf "Invalid syntax: %s, pos: %s" text (string_of_pos pos))

let extraneous_text_check ~end_markers =
  spaces
  >> get_pos
  >>= fun pos ->
  many_satisfy (fun c -> not (String.contains end_markers c))
  >>= fun s ->
  match s with "" -> return () | text -> invalid_syntax ~text ~pos

let result_of_mparser_result (x : 'a result) : ('a, string) CCResult.t =
  match x with
  | Success x -> Ok x
  | Failed (_, err) -> (
      match err with
      | No_error -> Error "Unknown error"
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
                         (Printf.sprintf "Unexpected: %s, pos: %s" s
                            (string_of_pos pos))
                     | Expected_error s ->
                       Some
                         (Printf.sprintf "Expected: %s, pos: %s" s
                            (string_of_pos pos))
                     | Message_error s -> Some s
                     | Compound_error (s, _) -> Some s
                     | Backtrack_error _ -> res
                     | Unknown_error -> res))
              None msgs
          with
          | None ->
            Error
              (Printf.sprintf "Unknown error, pos: %s" (string_of_pos pos))
          | Some s -> Error s))
