(* resolver.ml — static variable resolution pass (runs before evaluation). *)

type locals = (int, int) Hashtbl.t
type scope = (string, bool) Hashtbl.t
type function_kind = NoFunction | Function | Method | Initializer

type t = {
  locals : locals;
  scopes : scope Stack.t;
  mutable current_function : function_kind;
  mutable class_depth : int;
}

let make_resolver () =
  {
    locals = Hashtbl.create 64;
    scopes = Stack.create ();
    current_function = NoFunction;
    class_depth = 0;
  }

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

let resolve_local r uid name =
  let scopes = Stack.to_seq r.scopes |> Array.of_seq in
  let n = Array.length scopes in
  let rec loop i =
    if i >= n then ()
    else if Hashtbl.mem scopes.(i) name then Hashtbl.replace r.locals uid i
    else loop (i + 1)
  in
  loop 0

let rec resolve_function r kind params body line =
  let enclosing = r.current_function in
  r.current_function <- kind;
  begin_scope r;
  List.iter
    (fun p ->
      declare r p line;
      define r p)
    params;
  List.iter (resolve_stmt r) body;
  end_scope r;
  r.current_function <- enclosing

and resolve_expr r = function
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
  | Expr.This (line, uid) ->
      if r.class_depth = 0 then
        resolve_error line "this" "Can't use 'this' outside of a class.";
      resolve_local r uid "this"

and resolve_stmt r = function
  | Stmt.Expression e -> resolve_expr r e
  | Stmt.Print e -> resolve_expr r e
  | Stmt.Return (value, line) -> (
      match value with
      | Some e ->
          if r.current_function = NoFunction then
            resolve_error line "return" "Can't return from top-level code.";
          if r.current_function = Initializer then
            resolve_error line "return"
              "Can't return a value from an initializer.";
          resolve_expr r e
      | None ->
          if r.current_function = NoFunction then
            resolve_error line "return" "Can't return from top-level code.")
  | Stmt.VarDecl (name, init, line) ->
      declare r name line;
      (match init with Some e -> resolve_expr r e | None -> ());
      define r name
  | Stmt.FunDecl (name, params, body, line) ->
      declare r name line;
      define r name;
      resolve_function r Function params body line
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
  | Stmt.ClassDecl (name, superclass, methods, line) ->
      declare r name line;
      define r name;
      r.class_depth <- r.class_depth + 1;
      (match superclass with
      | Some (Expr.Variable (super_name, super_line, _)) as sc -> (
          if name = super_name then
            resolve_error super_line super_name
              "A class can't inherit from itself.";
          match sc with Some e -> resolve_expr r e | None -> ())
      | Some e -> resolve_expr r e
      | None -> ());
      List.iter
        (function
          | Stmt.FunDecl (mname, params, body, mline) ->
              let kind = if mname = "init" then Initializer else Method in
              let enclosing = r.current_function in
              r.current_function <- kind;
              begin_scope r;
              Hashtbl.replace (Stack.top r.scopes) "this" true;
              List.iter
                (fun p ->
                  declare r p mline;
                  define r p)
                params;
              List.iter (resolve_stmt r) body;
              end_scope r;
              r.current_function <- enclosing
          | _ -> ())
        methods;
      r.class_depth <- r.class_depth - 1

let resolve stmts =
  let r = make_resolver () in
  List.iter (resolve_stmt r) stmts;
  r.locals
