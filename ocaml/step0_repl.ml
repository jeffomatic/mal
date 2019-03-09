open Core

let read v = v

let eval v = v

let print v = v

let rep (v: string) = v |> read |> eval |> print

let rec loop () =
  print_string "user> " ;
  Out_channel.flush Out_channel.stdout ;
  match In_channel.input_line In_channel.stdin with
  | Some s -> s |> rep |> print_endline ; loop ()
  | None -> ()

let () = loop ()
