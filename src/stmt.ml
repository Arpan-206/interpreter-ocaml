(* stmt.ml — AST node types for Lox statements *)

type t =
  | Print of Expr.t (* print <expr>; *)
  | Expression of Expr.t (* <expr>; — expression used as statement *)
  | VarDecl of string * Expr.t option (* var name = <expr>?; *)
  | Block of t list (* { <stmt>* } — creates a new scope *)
  | If of Expr.t * t * t option (* if (<cond>) <then> else? <else> *)
  | While of Expr.t * t (* while (<cond>) <body> — for loops desugar to this *)
  | FunDecl of string * string list * t list (* fun name(params) { body } *)
  | Return of
      Expr.t
      option (* return <expr>?; — raises exception to unwind call stack *)
