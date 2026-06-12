type t =
  | Print of Expr.t
  | Expression of Expr.t
  | VarDecl of string * Expr.t option
  | Block of t list
  | If of Expr.t * t * t option
  | While of Expr.t * t
