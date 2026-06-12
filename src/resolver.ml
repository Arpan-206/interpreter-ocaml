(* resolver.ml — static variable resolution pass (runs before evaluation).
   Walks the entire AST and records, for each Variable and Assign expression,
   how many scope levels up their binding lives. This number (the "depth") is
   stored in a side table keyed by each node's unique uid.

   Why this is needed: without it, a closure always searches the live environment
   chain at call time, so a new variable declared after the closure is defined can
   shadow the one it was supposed to capture. The resolver fixes the binding at
   parse/resolve time so the evaluator jumps to exactly the right scope. *)

(* The output: uid → depth (0 = current scope, 1 = one level up, etc.) *)
type locals = (int, int) Hashtbl.t

(* One scope frame: variable name → has it been fully initialised yet?
   false = declared but not yet defined (catches `var x = x;` style errors) *)
type scope = (string, bool) Hashtbl.t

type t = {
  locals : locals;
  scopes : scope Stack.t; (* innermost scope is on top *)
}

let make_resolver () = { locals = Hashtbl.create 64; scopes = Stack.create () }

(* ── Scope management ───────────────────────────────────────────────────── *)

let begin_scope r = Stack.push (Hashtbl.create 8) r.scopes
let end_scope r = ignore (Stack.pop r.scopes)

(* Declare: name is known in this scope but not yet safe to read *)
let declare r name =
  if not (Stack.is_empty r.scopes) then
    Hashtbl.replace (Stack.top r.scopes) name false

(* Define: name is fully initialised and safe to read *)
let define r name =
  if not (Stack.is_empty r.scopes) then
    Hashtbl.replace (Stack.top r.scopes) name true

(* ── Core resolution ────────────────────────────────────────────────────── *)

(* Walk scopes from innermost outward looking for `name`.
   Record the hop count in locals under this node's uid.
   If not found in any local scope → it's a global, leave it unrecorded
   (the evaluator will fall back to a full env-chain walk for globals). *)
let resolve_local r uid name =
  (* Stack.to_seq yields top (innermost) first *)
  let scopes = Stack.to_seq r.scopes |> Array.of_seq in
  let n = Array.length scopes in
  let rec loop i =
    if i >= n then () (* not found locally → global *)
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
      (* Guard: catch `var x = x;` where x is declared but not yet defined *)
      (if not (Stack.is_empty r.scopes) then
         match Hashtbl.find_opt (Stack.top r.scopes) name with
         | Some false ->
             Printf.eprintf
               "[line %d] Error: Can't read local variable in its own \
                initializer.\n"
               line;
             exit 65
         | _ -> ());
      resolve_local r uid name
  | Expr.Assign (name, e, _, uid) ->
      (* Resolve the value expression first, then bind the target *)
      resolve_expr r e;
      resolve_local r uid name

(* ── Statement resolution ───────────────────────────────────────────────── *)

and resolve_stmt r = function
  | Stmt.Expression e -> resolve_expr r e
  | Stmt.Print e -> resolve_expr r e
  | Stmt.Return (Some e) -> resolve_expr r e
  | Stmt.Return None -> ()
  | Stmt.VarDecl (name, init) ->
      (* Declare first so the initialiser can't reference the variable itself *)
      declare r name;
      (match init with Some e -> resolve_expr r e | None -> ());
      define r name
  | Stmt.FunDecl (name, params, body) ->
      (* Define the function name before resolving its body so it can recurse *)
      declare r name;
      define r name;
      (* Function body gets its own scope for parameters *)
      begin_scope r;
      List.iter
        (fun p ->
          declare r p;
          define r p)
        params;
      List.iter (resolve_stmt r) body;
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

(* ── Public entry point ─────────────────────────────────────────────────── *)

(* Run the resolver over a full program and return the completed locals table *)
let resolve stmts =
  let r = make_resolver () in
  List.iter (resolve_stmt r) stmts;
  r.locals
