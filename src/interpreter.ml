type value = VBool of bool | VNil | VNum of float | VString of string

let runtime_error msg =
  Printf.eprintf "%s\n" msg;
  exit 70

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
  | Expr.Unary (op, e) -> (
      let v = eval e in
      match (op, v) with
      | Expr.Negate, VNum f -> VNum (-.f)
      | Expr.Not, VBool b -> VBool (not b)
      | Expr.Not, VNil -> VBool true (* !nil is truthy in Lox *)
      | Expr.Not, _ -> VBool false (* everything else is truthy *)
      | Expr.Negate, _ -> runtime_error "Operand must be a number.")
  | Expr.Binary (e1, op, e2) -> (
      let v1 = eval e1 in
      let v2 = eval e2 in
      match (v1, op, v2) with
      | VNum f1, Expr.Add, VNum f2 -> VNum (f1 +. f2)
      | VNum f1, Expr.Subtract, VNum f2 -> VNum (f1 -. f2)
      | VNum f1, Expr.Multiply, VNum f2 -> VNum (f1 *. f2)
      | VNum f1, Expr.Divide, VNum f2 ->
          if Float.equal f2 0. then runtime_error "Division by zero"
          else VNum (f1 /. f2)
      | VString s1, Expr.Add, VString s2 -> VString (s1 ^ s2)
      | ( VNum f1,
          (Expr.GREATER | Expr.GREATER_EQUAL | Expr.LESS | Expr.LESS_EQUAL),
          VNum f2 ) -> (
          let cmp =
            if Float.equal f1 f2 then 0 else if f1 < f2 then -1 else 1
          in
          match op with
          | Expr.GREATER -> VBool (cmp > 0)
          | Expr.GREATER_EQUAL -> VBool (cmp >= 0)
          | Expr.LESS -> VBool (cmp < 0)
          | Expr.LESS_EQUAL -> VBool (cmp <= 0)
          | _ -> assert false)
      | v1, (Expr.EQUAL | Expr.NOT_EQUAL), v2 -> (
          let eq =
            match (v1, v2) with
            | VNil, VNil -> true
            | VBool b1, VBool b2 -> b1 = b2
            | VNum f1, VNum f2 -> Float.equal f1 f2
            | VString s1, VString s2 -> s1 = s2
            | _ -> false
          in
          match op with
          | Expr.EQUAL -> VBool eq
          | Expr.NOT_EQUAL -> VBool (not eq)
          | _ -> assert false)
      | _, _, _ -> runtime_error "Operands must be two numbers or two strings.")
