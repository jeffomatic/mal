type maltype =
  | MalList of maltype list
  | MalNumber of int
  | MalSymbol of string
