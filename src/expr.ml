(* expr.ml — AST node types for Lox expressions *)

(* ── Unique ID counter ───────────────────────────────────────────────────── *)

(* Each Variable and Assign node gets a unique integer ID at parse time.
   The resolver uses these IDs as keys into the locals table (uid → depth),
   so two uses of the same variable name can be distinguished from each other. *)
let next_id = ref 0

let fresh_id () =
  incr next_id;
  !next_id

(* ── AST types ───────────────────────────────────────────────────────────── *)

type t =
  | Literal of literal
  | Grouping of t (* parenthesised expression *)
  | Unary of unary_op * t
  | Binary of t * binary_op * t
  | Variable of string * int * int (* name, line, uid *)
  | Assign of string * t * int * int (* name, value, line, uid *)
  | Or of t * t (* short-circuit logical or *)
  | And of t * t (* short-circuit logical and *)
  | Call of t * t list * int (* callee, args, line *)
  | Get of t * string * int (* object, property name, line *)
  | Set of t * string * t * int (* object, property name, value, line *)
  | This of int * int

and literal = LitBool of bool | LitNil | LitNum of float | LitStr of string

and unary_op =
  | Negate
  (* - *)
  | Not (* ! *)

and binary_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | EQUAL
  | NOT_EQUAL

(* ── Pretty printer ──────────────────────────────────────────────────────── *)

(* Format a float for display: integers show one decimal place (1 → "1.0"),
   other values use OCaml's default float string representation *)
let format_float f =
  if Float.is_integer f then Printf.sprintf "%.1f" f else string_of_float f

(* Print an S-expression representation of the AST — used by the "parse" command *)
let rec print = function
  | Literal (LitBool b) -> print_string (string_of_bool b)
  | Literal LitNil -> print_string "nil"
  | Literal (LitNum f) -> print_string (format_float f)
  | Literal (LitStr s) -> print_string s
  | Grouping e ->
      print_string "(group ";
      print e;
      print_string ")"
  | Unary (Negate, e) ->
      print_string "(- ";
      print e;
      print_string ")"
  | Unary (Not, e) ->
      print_string "(! ";
      print e;
      print_string ")"
  | Binary (l, op, r) ->
      let sym =
        match op with
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | GREATER -> ">"
        | GREATER_EQUAL -> ">="
        | LESS -> "<"
        | LESS_EQUAL -> "<="
        | EQUAL -> "=="
        | NOT_EQUAL -> "!="
      in
      print_string "(";
      print_string sym;
      print_string " ";
      print l;
      print_string " ";
      print r;
      print_string ")"
  | Variable (name, _, _) -> print_string name (* uid is internal only *)
  | Assign (name, e, _, _) ->
      print_string name;
      print_string " = ";
      print e
  | Or (l, r) ->
      print_string "(or ";
      print l;
      print_string " ";
      print r;
      print_string ")"
  | And (l, r) ->
      print_string "(and ";
      print l;
      print_string " ";
      print r;
      print_string ")"
  | Call (callee, args, _) ->
      print_string "(call ";
      print callee;
      List.iter
        (fun a ->
          print_string " ";
          print a)
        args;
      print_string ")"
  | Get (obj, name, _) ->
      print obj;
      print_string ".";
      print_string name
  | Set (obj, name, value, _) ->
      print obj;
      print_string ".";
      print_string name;
      print_string " = ";
      print value
  | This (_, _) -> print_string "this"
