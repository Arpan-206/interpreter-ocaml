(* serialise.ml — converts OCaml AST to bincode wire format for the Rust JIT *)

let buf = Buffer.create 4096
let write_u8 n = Buffer.add_char buf (Char.chr (n land 0xff))

let write_u32 n =
  write_u8 (n land 0xff);
  write_u8 ((n lsr 8) land 0xff);
  write_u8 ((n lsr 16) land 0xff);
  write_u8 ((n lsr 24) land 0xff)

let write_u64 n =
  write_u32 (n land 0xffffffff);
  write_u32 (Int64.to_int (Int64.shift_right_logical (Int64.of_int n) 32))

let write_f64 f =
  let bits = Int64.bits_of_float f in
  write_u32 (Int64.to_int (Int64.logand bits 0xffffffffL));
  write_u32 (Int64.to_int (Int64.shift_right_logical bits 32))

let write_string s =
  write_u64 (String.length s);
  Buffer.add_string buf s

let write_bool b = write_u8 (if b then 1 else 0)

let write_option f = function
  | None -> write_u8 0
  | Some x ->
      write_u8 1;
      f x

let write_list f xs =
  write_u64 (List.length xs);
  List.iter f xs

let write_unary_op = function
  | Expr.Negate -> write_u32 0
  | Expr.Not -> write_u32 1

let write_binary_op = function
  | Expr.Add -> write_u32 0
  | Expr.Subtract -> write_u32 1
  | Expr.Multiply -> write_u32 2
  | Expr.Divide -> write_u32 3
  | Expr.GREATER -> write_u32 4
  | Expr.GREATER_EQUAL -> write_u32 5
  | Expr.LESS -> write_u32 6
  | Expr.LESS_EQUAL -> write_u32 7
  | Expr.EQUAL -> write_u32 8
  | Expr.NOT_EQUAL -> write_u32 9

let rec write_expr = function
  | Expr.Literal (Expr.LitBool b) ->
      write_u32 0;
      write_bool b
  | Expr.Literal Expr.LitNil -> write_u32 1
  | Expr.Literal (Expr.LitNum f) ->
      write_u32 2;
      write_f64 f
  | Expr.Literal (Expr.LitStr s) ->
      write_u32 3;
      write_string s
  | Expr.Grouping e ->
      write_u32 4;
      write_expr e
  | Expr.Unary (op, e) ->
      write_u32 5;
      write_unary_op op;
      write_expr e
  | Expr.Binary (l, op, r) ->
      write_u32 6;
      write_expr l;
      write_binary_op op;
      write_expr r
  | Expr.Variable (name, line, uid) ->
      write_u32 7;
      write_string name;
      write_u32 line;
      write_u32 uid
  | Expr.Assign (name, e, line, uid) ->
      write_u32 8;
      write_string name;
      write_expr e;
      write_u32 line;
      write_u32 uid
  | Expr.Or (l, r) ->
      write_u32 9;
      write_expr l;
      write_expr r
  | Expr.And (l, r) ->
      write_u32 10;
      write_expr l;
      write_expr r
  | Expr.Call (callee, args, line) ->
      write_u32 11;
      write_expr callee;
      write_list write_expr args;
      write_u32 line
  | Expr.Get (obj, name, line) ->
      write_u32 12;
      write_expr obj;
      write_string name;
      write_u32 line
  | Expr.Set (obj, name, v, line) ->
      write_u32 13;
      write_expr obj;
      write_string name;
      write_expr v;
      write_u32 line
  | Expr.This (line, uid) ->
      write_u32 14;
      write_u32 line;
      write_u32 uid
  | Expr.Super (line, method_, uid) ->
      write_u32 15;
      write_u32 line;
      write_string method_;
      write_u32 uid

and write_stmt = function
  | Stmt.Print e ->
      write_u32 0;
      write_expr e
  | Stmt.Expression e ->
      write_u32 1;
      write_expr e
  | Stmt.VarDecl (name, init, line) ->
      write_u32 2;
      write_string name;
      write_option write_expr init;
      write_u32 line
  | Stmt.Block stmts ->
      write_u32 3;
      write_list write_stmt stmts
  | Stmt.If (cond, then_, else_) ->
      write_u32 4;
      write_expr cond;
      write_stmt then_;
      write_option write_stmt else_
  | Stmt.While (cond, body) ->
      write_u32 5;
      write_expr cond;
      write_stmt body
  | Stmt.FunDecl (name, params, body, line) ->
      write_u32 6;
      write_string name;
      write_list write_string params;
      write_list write_stmt body;
      write_u32 line
  | Stmt.Return (value, line) ->
      write_u32 7;
      write_option write_expr value;
      write_u32 line
  | Stmt.ClassDecl (name, super_, methods, line) ->
      write_u32 8;
      write_string name;
      write_option write_expr super_;
      write_list write_stmt methods;
      write_u32 line

let serialise stmts =
  Buffer.clear buf;
  (* bincode serialises Vec<T> as u64 length + elements *)
  write_u64 (List.length stmts);
  List.iter write_stmt stmts;
  Buffer.contents buf
