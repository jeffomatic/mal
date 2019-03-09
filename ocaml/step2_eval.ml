open Core

let primitive_add args _ =
  let sum =
    List.fold args ~init:0 ~f:(fun accum arg ->
        match arg with
        | Types.MalNumber n -> accum + n
        | _ -> failwith "invalid type" )
  in
  Types.MalNumber sum

let primitive_sub args _ =
  let a = List.nth_exn args 0 in
  let b = List.nth_exn args 1 in
  match (a, b) with
  | Types.MalNumber a, Types.MalNumber b -> Types.MalNumber (a - b)
  | _, _ -> failwith "invalid type"

let primitive_mul args _ =
  let a = List.nth_exn args 0 in
  let b = List.nth_exn args 1 in
  match (a, b) with
  | Types.MalNumber a, Types.MalNumber b -> Types.MalNumber (a * b)
  | _, _ -> failwith "invalid type"

let primitive_div args _ =
  let a = List.nth_exn args 0 in
  let b = List.nth_exn args 1 in
  match (a, b) with
  | Types.MalNumber a, Types.MalNumber b -> Types.MalNumber (a / b)
  | _, _ -> failwith "invalid type"

let rec eval_ast ast (env : Types.malenv) : Types.maltype =
  match ast with
  | Types.MalList ls -> (
    match ls with
    | [] -> MalList ls
    | hd :: tl -> (
        let hd = eval_ast hd env in
        let args = List.map tl ~f:(fun v -> eval_ast v env) in
        match hd with
        | Types.MalProc proc -> proc args env
        | _ -> failwith "not a proc" ) )
  | Types.MalSymbol sym -> String.Map.find_exn env sym
  | other -> other

let make_env () =
  let env = String.Map.empty in
  let env =
    String.Map.add_exn env ~key:"+" ~data:(Types.MalProc primitive_add)
  in
  let env =
    String.Map.add_exn env ~key:"-" ~data:(Types.MalProc primitive_sub)
  in
  let env =
    String.Map.add_exn env ~key:"*" ~data:(Types.MalProc primitive_mul)
  in
  let env =
    String.Map.add_exn env ~key:"/" ~data:(Types.MalProc primitive_div)
  in
  env

let read s = Reader.read_str s

let eval v =
  let env = make_env () in
  eval_ast v env

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
