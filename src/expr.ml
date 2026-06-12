type t =
  | Literal of literal
  | Grouping of t
  | Unary of unary_op * t
  | Binary of t * binary_op * t
  | Variable of string * int (* name + line *)
  | Assign of string * t * int (* name + value + line *)
  | Or of t * t
  | And of t * t

and literal =
  | LitBool of bool
  | LitNil
  | LitNum of float (* ← add this *)
  | LitStr of string (* ← and this, you'll need it soon *)

and unary_op = Negate | Not

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

let format_float f =
  if Float.is_integer f then Printf.sprintf "%.1f" f else string_of_float f

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
  | Variable (name, _) -> print_string name
  | Assign (name, e, _) ->
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
