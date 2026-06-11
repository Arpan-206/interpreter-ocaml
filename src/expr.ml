type t = Literal of literal | Grouping of t

and literal =
  | LitBool of bool
  | LitNil
  | LitNum of float (* ← add this *)
  | LitStr of string (* ← and this, you'll need it soon *)

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
