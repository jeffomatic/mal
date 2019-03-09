open Core
open Types

let rec pr_str form =
  match form with
  | MalList forms ->
      "(" ^ (List.map forms ~f:pr_str |> String.concat ~sep:" ") ^ ")"
  | MalNumber n -> string_of_int n
  | MalSymbol s -> s
