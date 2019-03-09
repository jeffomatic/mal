open Core

let read s = Reader.read_str s

let eval v = v

let print v = Printer.pr_str v

let rep (s : string) = s |> read |> eval |> print

let rec loop () =
  print_string "user> " ;
  Out_channel.flush Out_channel.stdout ;
  match In_channel.input_line In_channel.stdin with
  | Some s ->
      s |> rep |> print_endline ;
      loop ()
  | None -> ()

let () = loop ()
