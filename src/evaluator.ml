(* evaluator.ml — tree-walk interpreter for Lox
   Walks the AST produced by Parser, evaluating expressions and executing statements.
   Environments are chained hash tables (child → parent) for lexical scoping. *)

(* Exit with code 70 on runtime errors *)
let runtime_error msg =
  Printf.eprintf "%s\n" msg;
  exit 70

let runtime_error_at line msg =
  Printf.eprintf "%s\n[line %d]\n" msg line;
  exit 70

(* Used to unwind the call stack when a return statement is hit.
   Caught at the function call boundary in FunDecl. *)
exception Return of Value.t

(* Lox truthiness: nil and false are falsy, everything else is truthy *)
let is_truthy = function Value.VNil -> false | Value.VBool b -> b | _ -> true

(* ── Expression evaluator ───────────────────────────────────────────────── *)

let rec eval env = function
  | Expr.Literal (Expr.LitBool b) -> Value.VBool b
  | Expr.Literal Expr.LitNil -> Value.VNil
  | Expr.Literal (Expr.LitNum f) -> Value.VNum f
  | Expr.Literal (Expr.LitStr s) -> Value.VString s
  | Expr.Grouping e -> eval env e
  (* Variable lookup — walks up the environment chain *)
  | Expr.Variable (name, line) -> (
      match Env.get env name with
      | Ok v -> v
      | Error msg -> runtime_error_at line msg)
  (* Assignment — updates existing binding in the scope where it was defined *)
  | Expr.Assign (name, e, line) -> (
      let v = eval env e in
      match Env.assign env name v with
      | Ok () -> v
      | Error msg -> runtime_error_at line msg)
  | Expr.Unary (op, e) -> (
      let v = eval env e in
      match (op, v) with
      | Expr.Negate, Value.VNum f -> Value.VNum (-.f)
      | Expr.Not, Value.VBool b -> Value.VBool (not b)
      | Expr.Not, Value.VNil -> Value.VBool true (* !nil = true *)
      | Expr.Not, _ -> Value.VBool false (* !<truthy> = false *)
      | Expr.Negate, _ -> runtime_error "Operand must be a number.")
  (* Short-circuit or — returns the first truthy value, or the last value *)
  | Expr.Or (left, right) ->
      let v = eval env left in
      if is_truthy v then v else eval env right
  (* Short-circuit and — returns the first falsy value, or the last value *)
  | Expr.And (left, right) ->
      let v = eval env left in
      if not (is_truthy v) then v else eval env right
  (* Function call — evaluates callee and args, checks arity, dispatches *)
  | Expr.Call (callee, args, line) ->
      let fn = eval env callee in
      let arg_vals = List.map (eval env) args in
      let arity, call =
        match fn with
        | Value.VCallable c -> (c.arity, c.call)
        | Value.VFun f -> (f.arity, f.call)
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
          (* Equality is defined across types — different types are never equal *)
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
  | Stmt.Expression expr ->
      (* Evaluate for side effects, discard result *)
      ignore (eval env expr)
  | Stmt.VarDecl (name, init) ->
      let v =
        match init with Some expr -> eval env expr | None -> Value.VNil
      in
      Env.define env name v
  | Stmt.Block stmts ->
      (* Each block creates a fresh child environment for its scope *)
      let child_env = Env.make_child env in
      List.iter (exec child_env) stmts
  | Stmt.If (condition, then_branch, else_branch) -> (
      let value = eval env condition in
      if is_truthy value then exec env then_branch
      else match else_branch with Some s -> exec env s | None -> ())
  | Stmt.While (condition, body) ->
      (* for loops are desugared to while at parse time *)
      while is_truthy (eval env condition) do
        exec env body
      done
  (* Raise Return exception to unwind the call stack back to the call site *)
  | Stmt.Return expr ->
      let v = match expr with Some e -> eval env e | None -> Value.VNil in
      raise (Return v)
  | Stmt.FunDecl (name, params, body) ->
      (* Capture the current environment as the closure at definition time,
         not at call time — this is what makes closures work correctly *)
      let closure = env in
      Env.define env name
        (Value.VFun
           {
             arity = List.length params;
             name;
             call =
               (fun args ->
                 (* Each call gets a fresh child of the closure env *)
                 let fn_env = Env.make_child closure in
                 List.iter2 (Env.define fn_env) params args;
                 (* Execute body, catching Return to get the return value *)
                 let result = ref Value.VNil in
                 (try List.iter (exec fn_env) body
                  with Return v -> result := v);
                 !result);
           })

(* ── Program entry point ────────────────────────────────────────────────── *)

let exec_program stmts =
  let env = Env.make () in
  (* Seed global environment with native functions *)
  Env.define env "clock"
    (Value.VCallable
       {
         arity = 0;
         name = "clock";
         (* Returns seconds since Unix epoch as a float *)
         call = (fun _ -> Value.VNum (Unix.gettimeofday ()));
       });
  List.iter (exec env) stmts
