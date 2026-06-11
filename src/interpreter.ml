type value = VBool of bool | VNil | VNum of float | VString of string

let print_value = function
  | VBool true -> print_string "true"
  | VBool false -> print_string "false"
  | VNil -> print_string "nil"
  | VNum f ->
      if Float.is_integer f then print_string (string_of_int (int_of_float f))
      else print_string (string_of_float f)
  | VString s -> print_string s

let rec eval = function
  | Expr.Literal (Expr.LitBool b) -> VBool b
  | Expr.Literal Expr.LitNil -> VNil
  | Expr.Literal (Expr.LitNum f) -> VNum f
  | Expr.Literal (Expr.LitStr s) -> VString s
  | Expr.Grouping e -> eval e
  | Expr.Unary _ -> failwith "unary not yet implemented"
  | Expr.Binary _ -> failwith "binary not yet implemented"
