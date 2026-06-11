type t = VBool of bool | VNil | VNum of float | VString of string

let print = function
  | VBool true -> print_string "true"
  | VBool false -> print_string "false"
  | VNil -> print_string "nil"
  | VNum f ->
      if Float.is_integer f then print_string (string_of_int (int_of_float f))
      else print_string (string_of_float f)
  | VString s -> print_string s
