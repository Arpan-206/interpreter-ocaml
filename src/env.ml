type t = { vars : (string, Value.t) Hashtbl.t; parent : t option (* ← add *) }

let make () = { vars = Hashtbl.create 16; parent = None }
let make_child parent = { vars = Hashtbl.create 8; parent = Some parent }
let define env name value = Hashtbl.replace env.vars name value

let rec get env name =
  match Hashtbl.find_opt env.vars name with
  | Some v -> v
  | None -> (
      match env.parent with
      | Some p -> get p name (* walk up the scope chain *)
      | None ->
          Printf.eprintf "Undefined variable '%s'.\n" name;
          exit 70)

let rec assign env name value =
  if Hashtbl.mem env.vars name then Hashtbl.replace env.vars name value
  else
    match env.parent with
    | Some p -> assign p name value (* assign in the scope where it's defined *)
    | None ->
        Printf.eprintf "Undefined variable '%s'.\n" name;
        exit 70
