open Core
open Types

let rec pr_str v =
  match v with
  | MalList vals ->
      "(" ^ (List.map vals ~f:pr_str |> String.concat ~sep:" ") ^ ")"
  | MalNumber n -> string_of_int n
  | MalSymbol s -> s
  | MalProc _ -> "proc"

let rec debug v =
  match v with
  | MalList vals ->
      "list:(" ^ (List.map vals ~f:debug |> String.concat ~sep:" ") ^ ")"
  | MalNumber n -> "number:" ^ string_of_int n
  | MalSymbol s -> "symbol:" ^ s
  | MalProc _ -> "proc"
