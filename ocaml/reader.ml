open Core
open Types

let peek (tokens : string list) = List.hd tokens

let next (tokens : string list) = (List.hd_exn tokens, List.tl_exn tokens)

let token_pat =
  {|[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)|}

let token_rex = Pcre.regexp token_pat

let tokenize s =
  let substrings = Pcre.extract_all ~full_match:false ~rex:token_rex s in
  Array.map substrings ~f:(fun a -> Array.nget a 0) |> Array.to_list

let is_int_string s =
  try int_of_string s |> string_of_int = s with Failure _ -> false

let rec read_form tokens =
  match peek tokens with
  | None -> failwith "token expected"
  | Some tok -> if tok.[0] = '(' then read_list tokens else read_atom tokens

and read_list tokens =
  let _, tokens = next tokens in
  let rec aux tokens accum =
    match peek tokens with
    | None -> failwith "closing ')' expected"
    | Some tok ->
        if tok = ")" then
          let _, tokens = next tokens in
          let v = MalList (List.rev accum) in
          (v, tokens)
        else
          let form, tokens = read_form tokens in
          aux tokens (form :: accum)
  in
  aux tokens []

and read_atom tokens =
  let tok, tokens = next tokens in
  if is_int_string tok then
    let atom = MalNumber (int_of_string tok) in
    (atom, tokens)
  else
    let atom = MalSymbol tok in
    (atom, tokens)

let read_str s =
  let form, _ = s |> tokenize |> read_form in
  form
