(* evaluator.ml — tree-walk interpreter for Lox *)

let runtime_error msg =
  Printf.eprintf "%s\n" msg;
  exit 70

let runtime_error_at line msg =
  Printf.eprintf "%s\n[line %d]\n" msg line;
  exit 70

exception Return of Value.t

let is_truthy = function Value.VNil -> false | Value.VBool b -> b | _ -> true
let locals : (int, int) Hashtbl.t ref = ref (Hashtbl.create 0)
let global_env : Env.t option ref = ref None

let rec find_method cls name =
  match Hashtbl.find_opt cls.Value.methods name with
  | Some id -> Some id
  | None -> (
      match cls.Value.superclass with
      | Some super -> find_method super name
      | None -> None)

type lox_fun = {
  lf_arity : int;
  lf_name : string;
  lf_params : string list;
  lf_body : Stmt.t list;
  lf_closure : Env.t;
  lf_is_initializer : bool;
}

let fun_table : (int, lox_fun) Hashtbl.t = Hashtbl.create 32
let next_fun_id = ref 0

let register_fun lf =
  incr next_fun_id;
  let id = !next_fun_id in
  Hashtbl.replace fun_table id lf;
  id

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

let rec call_fun lf this_opt args =
  let fn_env = Env.make_child lf.lf_closure in
  (match this_opt with
  | Some inst -> Env.define fn_env "this" inst
  | None -> ());
  List.iter2 (Env.define fn_env) lf.lf_params args;
  let result = ref Value.VNil in
  (try
     List.iter (exec fn_env) lf.lf_body;
     if lf.lf_is_initializer then
       match this_opt with
       | Some inst -> result := inst
       | None -> runtime_error "Initializer called without `this`."
   with Return v ->
     if lf.lf_is_initializer then
       match this_opt with
       | Some inst -> result := inst
       | None -> runtime_error "Initializer called without `this`."
     else result := v);
  !result

and make_fun_callable lf =
  Value.VFun
    {
      Value.arity = lf.lf_arity;
      name = lf.lf_name;
      call = (fun args -> call_fun lf None args);
    }

and make_bound_method lf inst =
  Value.VFun
    {
      Value.arity = lf.lf_arity;
      name = lf.lf_name;
      call =
        (fun args -> call_fun lf (Some (Value.VInstance inst)) args);
    }

and eval env = function
  | Expr.Literal (Expr.LitBool b) -> Value.VBool b
  | Expr.Literal Expr.LitNil -> Value.VNil
  | Expr.Literal (Expr.LitNum f) -> Value.VNum f
  | Expr.Literal (Expr.LitStr s) -> Value.VString s
  | Expr.Grouping e -> eval env e
  | Expr.Variable (name, line, uid) -> lookup_var env name line uid
  | Expr.This (line, uid) -> lookup_var env "this" line uid
  | Expr.Super (line, method_name, uid) -> (
      match Hashtbl.find_opt !locals uid with
      | None -> runtime_error_at line "Undefined 'super'."
      | Some distance -> (
          match Env.get_at env distance "super" with
          | Some (Value.VClass superclass) -> (
              match Env.get_at env (distance - 1) "this" with
              | Some (Value.VInstance instance) -> (
                  match find_method superclass method_name with
                  | Some method_id ->
                      let lf = Hashtbl.find fun_table method_id in
                      make_bound_method lf instance
                  | None ->
                      runtime_error_at line
                        (Printf.sprintf "Undefined property '%s'." method_name))
              | _ -> runtime_error_at line "Undefined 'this' in super lookup.")
          | _ -> runtime_error_at line "Superclass must be a class."))
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
          match List.assoc_opt name inst.fields with
          | Some v -> v
          | None -> (
              match find_method inst.instance_class name with
              | Some method_id ->
                  let lf = Hashtbl.find fun_table method_id in
                  make_bound_method lf inst
              | None ->
                  runtime_error_at line
                    (Printf.sprintf "Undefined property '%s'." name)))
      | _ -> runtime_error_at line "Only instances have properties.")
  | Expr.Set (obj, name, value, line) -> (
      match eval env obj with
      | Value.VInstance inst ->
          let v = eval env value in
          let rec update = function
            | [] -> [ (name, v) ]
            | (k, _) :: tl when k = name -> (name, v) :: tl
            | kv :: tl -> kv :: update tl
          in
          inst.fields <- update inst.fields;
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
  | Expr.Call (Expr.Get (obj_expr, method_name, get_line), args, _line) -> (
      match eval env obj_expr with
      | Value.VInstance inst -> (
          match List.assoc_opt method_name inst.fields with
          | Some (Value.VCallable c) ->
              let arg_vals = List.map (eval env) args in
              if List.length arg_vals <> c.arity then
                runtime_error_at get_line
                  (Printf.sprintf "Expected %d arguments but got %d." c.arity
                     (List.length arg_vals))
              else c.call arg_vals
          | Some (Value.VFun f) ->
              let arg_vals = List.map (eval env) args in
              if List.length arg_vals <> f.arity then
                runtime_error_at get_line
                  (Printf.sprintf "Expected %d arguments but got %d." f.arity
                     (List.length arg_vals))
              else f.call arg_vals
          | Some _ ->
              runtime_error_at get_line "Can only call functions and classes."
          | None -> (
              match find_method inst.instance_class method_name with
              | Some method_id ->
                  let lf = Hashtbl.find fun_table method_id in
                  let arg_vals = List.map (eval env) args in
                  if List.length arg_vals <> lf.lf_arity then
                    runtime_error_at get_line
                      (Printf.sprintf "Expected %d arguments but got %d."
                         lf.lf_arity (List.length arg_vals))
                  else
                    call_fun lf (Some (Value.VInstance inst)) arg_vals
              | None ->
                  runtime_error_at get_line
                    (Printf.sprintf "Undefined property '%s'." method_name)))
      | other -> (
          let fn =
            match other with
            | Value.VInstance _ -> assert false
            | _ ->
                runtime_error_at get_line "Only instances have properties."
          in
          ignore fn))
  | Expr.Call (callee, args, line) -> (
      let fn = eval env callee in
      let arg_vals = List.map (eval env) args in
      let nargs = List.length arg_vals in
      match fn with
      | Value.VCallable c ->
          if nargs <> c.arity then
            runtime_error_at line
              (Printf.sprintf "Expected %d arguments but got %d." c.arity nargs)
          else c.call arg_vals
      | Value.VFun f ->
          if nargs <> f.arity then
            runtime_error_at line
              (Printf.sprintf "Expected %d arguments but got %d." f.arity nargs)
          else f.call arg_vals
      | Value.VClass c ->
          let init = find_method c "init" in
          let init_arity =
            match init with
            | Some id -> (Hashtbl.find fun_table id).lf_arity
            | None -> 0
          in
          if nargs <> init_arity then
            runtime_error_at line
              (Printf.sprintf "Expected %d arguments but got %d." init_arity nargs)
          else
            let inst = Value.{ instance_class = c; fields = [] } in
            (match init with
            | Some id ->
                let lf = Hashtbl.find fun_table id in
                ignore (call_fun lf (Some (Value.VInstance inst)) arg_vals)
            | None -> ());
            Value.VInstance inst
      | _ -> runtime_error_at line "Can only call functions and classes.")
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
            | Value.VString s1, Value.VString s2 -> String.equal s1 s2
            | _ -> false
          in
          match op with
          | Expr.EQUAL -> Value.VBool eq
          | Expr.NOT_EQUAL -> Value.VBool (not eq)
          | _ -> assert false)
      | _ -> runtime_error "Operands must be two numbers or two strings.")

and exec env = function
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
  | Stmt.Return (value, _) ->
      let v = match value with Some e -> eval env e | None -> Value.VNil in
      raise (Return v)
  | Stmt.FunDecl (name, params, body, _) ->
      let lf =
        {
          lf_arity = List.length params;
          lf_name = name;
          lf_params = params;
          lf_body = body;
          lf_closure = env;
          lf_is_initializer = false;
        }
      in
      Env.define env name (make_fun_callable lf)
  | Stmt.ClassDecl (name, superclass_expr, methods, _) -> (
      let superclass =
        match superclass_expr with
        | None -> None
        | Some expr -> (
            let v = eval env expr in
            match v with
            | Value.VClass c -> Some c
            | _ -> runtime_error "Superclass must be a class.")
      in
      Env.define env name Value.VNil;
      let method_env =
        match superclass with
        | Some sc ->
            let child = Env.make_child env in
            Env.define child "super" (Value.VClass sc);
            child
        | None -> env
      in
      let method_table = Hashtbl.create 8 in
      List.iter
        (function
          | Stmt.FunDecl (mname, params, body, _) ->
              let lf =
                {
                  lf_arity = List.length params;
                  lf_name = mname;
                  lf_params = params;
                  lf_body = body;
                  lf_closure = method_env;
                  lf_is_initializer = mname = "init";
                }
              in
              let id = register_fun lf in
              Hashtbl.replace method_table mname id
          | _ -> ())
        methods;
      let klass =
        Value.VClass { class_name = name; methods = method_table; superclass }
      in
      match Env.assign env name klass with
      | Ok () -> ()
      | Error msg -> runtime_error msg)

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
