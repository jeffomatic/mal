open Core

type maltype =
  | MalList of mallist
  | MalNumber of int
  | MalSymbol of string
  | MalProc of malproc

and mallist = maltype list

and malenv = maltype String.Map.t

and malproc = mallist -> malenv -> maltype
