type t = { vars : (string, Value.t) Hashtbl.t }

let make () = { vars = Hashtbl.create 16 }
let define env name value = Hashtbl.replace env.vars name value

let get env name =
  match Hashtbl.find_opt env.vars name with
  | Some v -> v
  | None ->
      Printf.eprintf "Undefined variable '%s'.\n" name;
      exit 70

let assign env name value =
  if Hashtbl.mem env.vars name then Hashtbl.replace env.vars name value
  else (
    Printf.eprintf "Undefined variable '%s'.\n" name;
    exit 70)
