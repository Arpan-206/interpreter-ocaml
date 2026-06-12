(* evaluator.ml — tree-walk interpreter for Lox
   Walks the AST produced by Parser, evaluating expressions and executing statements.
   Environments are chained hash tables (child → parent) for lexical scoping.
   A separate resolver pass populates `locals` before execution begins. *)

let runtime_error msg =
  Printf.eprintf "%s\n" msg;
  exit 70

let runtime_error_at line msg =
  Printf.eprintf "%s\n[line %d]\n" msg line;
  exit 70

exception Return of Value.t

let is_truthy = function Value.VNil -> false | Value.VBool b -> b | _ -> true

(* Populated by the resolver before exec_program is called.
   Maps each Variable/Assign node's uid → scope depth (hops to defining env). *)
let locals : (int, int) Hashtbl.t ref = ref (Hashtbl.create 0)

(* Set once in exec_program to the top-level global environment.
   Used by lookup_var / Assign when a variable has no resolver entry (i.e. it is
   a true global), so we bypass the live closure chain and always read/write the
   correct global binding — even when a same-named block-local exists at runtime
   in an enclosing scope of the closure. *)
let global_env : Env.t option ref = ref None

let lookup_var env name line uid =
  match Hashtbl.find_opt !locals uid with
  | Some depth -> (
      match Env.get_at env depth name with
      | Some v -> v
      | None -> runtime_error_at line ("Undefined variable '" ^ name ^ "'."))
  | None -> (
      let target = match !global_env with Some g -> g | None -> env in
      match Env.get target name with
      | Ok v -> v
      | Error msg -> runtime_error_at line msg)

(* ── Expression evaluator ───────────────────────────────────────────────── *)

let rec eval env = function
  | Expr.Literal (Expr.LitBool b) -> Value.VBool b
  | Expr.Literal Expr.LitNil -> Value.VNil
  | Expr.Literal (Expr.LitNum f) -> Value.VNum f
  | Expr.Literal (Expr.LitStr s) -> Value.VString s
  | Expr.Grouping e -> eval env e
  | Expr.Variable (name, line, uid) -> lookup_var env name line uid
  | Expr.Assign (name, e, line, uid) ->
      let v = eval env e in
      (match Hashtbl.find_opt !locals uid with
      | Some depth -> Env.assign_at env depth name v
      | None -> (
          let target = match !global_env with Some g -> g | None -> env in
          match Env.assign target name v with
          | Ok () -> ()
          | Error msg -> runtime_error_at line msg));
      v
  | Expr.Get (obj, name, line) -> (
      match eval env obj with
      | Value.VInstance inst -> (
          match Hashtbl.find_opt inst.fields name with
          | Some v -> v
          | None ->
              runtime_error_at line
                (Printf.sprintf "Undefined property '%s'." name))
      | _ -> runtime_error_at line "Only instances have properties.")
  | Expr.Set (obj, name, value, line) -> (
      match eval env obj with
      | Value.VInstance inst ->
          let v = eval env value in
          Hashtbl.replace inst.fields name v;
          v
      | _ -> runtime_error_at line "Only instances have fields.")
  | Expr.Unary (op, e) -> (
      let v = eval env e in
      match (op, v) with
      | Expr.Negate, Value.VNum f -> Value.VNum (-.f)
      | Expr.Not, Value.VBool b -> Value.VBool (not b)
      | Expr.Not, Value.VNil -> Value.VBool true
      | Expr.Not, _ -> Value.VBool false
      | Expr.Negate, _ -> runtime_error "Operand must be a number.")
  | Expr.Or (left, right) ->
      let v = eval env left in
      if is_truthy v then v else eval env right
  | Expr.And (left, right) ->
      let v = eval env left in
      if not (is_truthy v) then v else eval env right
  | Expr.Call (callee, args, line) ->
      let fn = eval env callee in
      let arg_vals = List.map (eval env) args in
      let arity, call =
        match fn with
        | Value.VCallable c -> (c.arity, c.call)
        | Value.VFun f -> (f.arity, f.call)
        | Value.VClass c ->
            ( 0,
              fun _ ->
                Value.VInstance
                  { instance_class = c; fields = Hashtbl.create 4 } )
        | _ -> runtime_error_at line "Can only call functions and classes."
      in
      if List.length arg_vals <> arity then
        runtime_error_at line
          (Printf.sprintf "Expected %d arguments but got %d." arity
             (List.length arg_vals))
      else call arg_vals
  | Expr.Binary (e1, op, e2) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match (v1, op, v2) with
      | Value.VNum f1, Expr.Add, Value.VNum f2 -> Value.VNum (f1 +. f2)
      | Value.VNum f1, Expr.Subtract, Value.VNum f2 -> Value.VNum (f1 -. f2)
      | Value.VNum f1, Expr.Multiply, Value.VNum f2 -> Value.VNum (f1 *. f2)
      | Value.VNum f1, Expr.Divide, Value.VNum f2 ->
          if Float.equal f2 0. then runtime_error "Division by zero."
          else Value.VNum (f1 /. f2)
      | Value.VString s1, Expr.Add, Value.VString s2 -> Value.VString (s1 ^ s2)
      | ( Value.VNum f1,
          (Expr.GREATER | Expr.GREATER_EQUAL | Expr.LESS | Expr.LESS_EQUAL),
          Value.VNum f2 ) -> (
          let cmp = Float.compare f1 f2 in
          match op with
          | Expr.GREATER -> Value.VBool (cmp > 0)
          | Expr.GREATER_EQUAL -> Value.VBool (cmp >= 0)
          | Expr.LESS -> Value.VBool (cmp < 0)
          | Expr.LESS_EQUAL -> Value.VBool (cmp <= 0)
          | _ -> assert false)
      | v1, (Expr.EQUAL | Expr.NOT_EQUAL), v2 -> (
          let eq =
            match (v1, v2) with
            | Value.VNil, Value.VNil -> true
            | Value.VBool b1, Value.VBool b2 -> b1 = b2
            | Value.VNum f1, Value.VNum f2 -> Float.equal f1 f2
            | Value.VString s1, Value.VString s2 -> s1 = s2
            | _ -> false
          in
          match op with
          | Expr.EQUAL -> Value.VBool eq
          | Expr.NOT_EQUAL -> Value.VBool (not eq)
          | _ -> assert false)
      | _ -> runtime_error "Operands must be two numbers or two strings.")

(* ── Statement executor ─────────────────────────────────────────────────── *)

let rec exec env = function
  | Stmt.Print expr ->
      Value.print (eval env expr);
      print_newline ()
  | Stmt.Expression expr -> ignore (eval env expr)
  | Stmt.VarDecl (name, init, _) ->
      let v = match init with Some e -> eval env e | None -> Value.VNil in
      Env.define env name v
  | Stmt.Block stmts ->
      let child_env = Env.make_child env in
      List.iter (exec child_env) stmts
  | Stmt.If (condition, then_branch, else_branch) -> (
      let value = eval env condition in
      if is_truthy value then exec env then_branch
      else match else_branch with Some s -> exec env s | None -> ())
  | Stmt.While (condition, body) ->
      while is_truthy (eval env condition) do
        exec env body
      done
  | Stmt.Return expr ->
      let v = match expr with Some e -> eval env e | None -> Value.VNil in
      raise (Return v)
  | Stmt.FunDecl (name, params, body, _) ->
      let closure = env in
      Env.define env name
        (Value.VFun
           {
             arity = List.length params;
             name;
             call =
               (fun args ->
                 let fn_env = Env.make_child closure in
                 List.iter2 (Env.define fn_env) params args;
                 let result = ref Value.VNil in
                 (try List.iter (exec fn_env) body
                  with Return v -> result := v);
                 !result);
           })
  | Stmt.ClassDecl (name, _) ->
      Env.define env name (Value.VClass { class_name = name })

(* ── Program entry point ────────────────────────────────────────────────── *)

let exec_program stmts =
  let env = Env.make () in
  global_env := Some env;
  Env.define env "clock"
    (Value.VCallable
       {
         arity = 0;
         name = "clock";
         call = (fun _ -> Value.VNum (Unix.gettimeofday ()));
       });
  List.iter (exec env) stmts
