(* env.ml — list-based lexical environment, optimized for small scopes *)

type t = { mutable vars : (string * Value.t) list; parent : t option }

let make () = { vars = []; parent = None }
let make_child parent = { vars = []; parent = Some parent }
let define env name value = env.vars <- (name, value) :: env.vars

let rec get env name =
  match List.assoc_opt name env.vars with
  | Some v -> Ok v
  | None -> (
      match env.parent with
      | Some p -> get p name
      | None -> Error (Printf.sprintf "Undefined variable '%s'." name))

let rec assign env name value =
  let rec update = function
    | [] -> false
    | (k, _) :: tl when k = name ->
        env.vars <- (name, value) :: tl;
        true
    | _ :: tl -> update tl
  in
  if update env.vars then Ok ()
  else
    match env.parent with
    | Some p -> assign p name value
    | None -> Error (Printf.sprintf "Undefined variable '%s'." name)

let rec ancestor env n =
  if n = 0 then env
  else
    match env.parent with
    | Some p -> ancestor p (n - 1)
    | None -> failwith "Resolver scope depth mismatch"

let get_at env n name = List.assoc_opt name (ancestor env n).vars

let assign_at env n name v =
  let target = ancestor env n in
  let rec update = function
    | [] -> target.vars <- (name, v) :: target.vars
    | (k, _) :: tl when k = name -> target.vars <- (name, v) :: tl
    | _ :: tl -> update tl
  in
  update target.vars
