(* resolver.ml — static variable resolution pass (runs before evaluation).
   Walks the entire AST and records, for each Variable and Assign expression,
   how many scope levels up their binding lives. This number (the "depth") is
   stored in a side table keyed by each node's unique uid.

   Why this is needed: without it, a closure always searches the live environment
   chain at call time, so a new variable declared after the closure is defined can
   shadow the one it was supposed to capture. The resolver fixes the binding at
   parse/resolve time so the evaluator jumps to exactly the right scope. *)

type locals = (int, int) Hashtbl.t
type scope = (string, bool) Hashtbl.t

type t = {
  locals : locals;
  scopes : scope Stack.t;
  mutable function_depth : int; (* 0 = top-level, >0 = inside a function *)
}

let make_resolver () =
  { locals = Hashtbl.create 64; scopes = Stack.create (); function_depth = 0 }

(* ── Scope management ───────────────────────────────────────────────────── *)

let begin_scope r = Stack.push (Hashtbl.create 8) r.scopes
let end_scope r = ignore (Stack.pop r.scopes)

let resolve_error line name msg =
  Printf.eprintf "[line %d] Error at '%s': %s\n" line name msg;
  exit 65

let declare r name line =
  if not (Stack.is_empty r.scopes) then begin
    let scope = Stack.top r.scopes in
    if Hashtbl.mem scope name then
      resolve_error line name "Already a variable with this name in this scope.";
    Hashtbl.replace scope name false
  end

let define r name =
  if not (Stack.is_empty r.scopes) then
    Hashtbl.replace (Stack.top r.scopes) name true

(* ── Core resolution ────────────────────────────────────────────────────── *)

let resolve_local r uid name =
  let scopes = Stack.to_seq r.scopes |> Array.of_seq in
  let n = Array.length scopes in
  let rec loop i =
    if i >= n then ()
    else if Hashtbl.mem scopes.(i) name then Hashtbl.replace r.locals uid i
    else loop (i + 1)
  in
  loop 0

(* ── Expression resolution ──────────────────────────────────────────────── *)

let rec resolve_expr r = function
  | Expr.Literal _ -> ()
  | Expr.Grouping e -> resolve_expr r e
  | Expr.Unary (_, e) -> resolve_expr r e
  | Expr.Binary (l, _, right) ->
      resolve_expr r l;
      resolve_expr r right
  | Expr.Or (l, right) | Expr.And (l, right) ->
      resolve_expr r l;
      resolve_expr r right
  | Expr.Call (callee, args, _) ->
      resolve_expr r callee;
      List.iter (resolve_expr r) args
  | Expr.Variable (name, line, uid) ->
      (if not (Stack.is_empty r.scopes) then
         match Hashtbl.find_opt (Stack.top r.scopes) name with
         | Some false ->
             resolve_error line name
               "Can't read local variable in its own initializer."
         | _ -> ());
      resolve_local r uid name
  | Expr.Assign (name, e, _, uid) ->
      resolve_expr r e;
      resolve_local r uid name
  | Expr.Get (obj, _, _) -> resolve_expr r obj
  | Expr.Set (obj, _, value, _) ->
      resolve_expr r obj;
      resolve_expr r value

(* ── Statement resolution ───────────────────────────────────────────────── *)

and resolve_stmt r = function
  | Stmt.Expression e -> resolve_expr r e
  | Stmt.Print e -> resolve_expr r e
  | Stmt.Return (Some e) ->
      if r.function_depth = 0 then
        resolve_error 0 "return" "Can't return from top-level code.";
      resolve_expr r e
  | Stmt.Return None ->
      if r.function_depth = 0 then
        resolve_error 0 "return" "Can't return from top-level code."
  | Stmt.VarDecl (name, init, line) ->
      declare r name line;
      (match init with Some e -> resolve_expr r e | None -> ());
      define r name
  | Stmt.FunDecl (name, params, body, line) ->
      declare r name line;
      define r name;
      begin_scope r;
      List.iter
        (fun p ->
          declare r p line;
          define r p)
        params;
      (* Increment depth for the body — return is valid here *)
      r.function_depth <- r.function_depth + 1;
      List.iter (resolve_stmt r) body;
      r.function_depth <- r.function_depth - 1;
      end_scope r
  | Stmt.Block stmts ->
      begin_scope r;
      List.iter (resolve_stmt r) stmts;
      end_scope r
  | Stmt.If (cond, then_, else_) -> (
      resolve_expr r cond;
      resolve_stmt r then_;
      match else_ with Some s -> resolve_stmt r s | None -> ())
  | Stmt.While (cond, body) ->
      resolve_expr r cond;
      resolve_stmt r body
  | Stmt.ClassDecl (name, line) ->
      declare r name line;
      define r name
(* ── Public entry point ─────────────────────────────────────────────────── *)

let resolve stmts =
  let r = make_resolver () in
  List.iter (resolve_stmt r) stmts;
  r.locals
