type t =
  | Print of Expr.t
  | Expression of Expr.t
  | VarDecl of string * Expr.t option
