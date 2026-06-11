type t = Literal of literal
and literal = LitBool of bool | LitNil

let print = function
  | Literal (LitBool b) -> print_string (string_of_bool b)
  | Literal LitNil -> print_string "nil"
