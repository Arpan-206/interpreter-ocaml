(* env.ml — lexical environment for variable storage
   Environments form a chain of scopes: each child holds a reference to its
   parent, allowing variable lookup and assignment to walk up to enclosing scopes. *)

type t = {
  vars : (string, Value.t) Hashtbl.t;
  parent : t option; (* None only at the global scope *)
}

(* Create the global (top-level) environment *)
let make () = { vars = Hashtbl.create 16; parent = None }

(* Create a child scope — used for blocks, function calls, closures *)
let make_child parent = { vars = Hashtbl.create 8; parent = Some parent }

(* Define a new variable in the current scope (shadows any outer binding) *)
let define env name value = Hashtbl.replace env.vars name value

(* Look up a variable — walks up the scope chain until found or hits global *)
let rec get env name =
  match Hashtbl.find_opt env.vars name with
  | Some v -> Ok v
  | None -> (
      match env.parent with
      | Some p -> get p name
      | None -> Error (Printf.sprintf "Undefined variable '%s'." name))

(* Assign to an existing variable — updates the scope where it was defined,
   not necessarily the current one. Error if not found anywhere in the chain. *)
let rec assign env name value =
  if Hashtbl.mem env.vars name then (
    Hashtbl.replace env.vars name value;
    Ok ())
  else
    match env.parent with
    | Some p -> assign p name value
    | None -> Error (Printf.sprintf "Undefined variable '%s'." name)
