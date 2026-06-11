type t = { vars : (string, Value.t) Hashtbl.t; parent : t option (* ← add *) }

let make () = { vars = Hashtbl.create 16; parent = None }
let make_child parent = { vars = Hashtbl.create 8; parent = Some parent }
let define env name value = Hashtbl.replace env.vars name value

let rec get env name =
  match Hashtbl.find_opt env.vars name with
  | Some v -> Ok v
  | None -> (
      match env.parent with
      | Some p -> get p name
      | None -> Error (Printf.sprintf "Undefined variable '%s'." name))

let rec assign env name value =
  if Hashtbl.mem env.vars name then (
    Hashtbl.replace env.vars name value;
    Ok ())
  else
    match env.parent with
    | Some p -> assign p name value
    | None -> Error (Printf.sprintf "Undefined variable '%s'." name)
