type parser = { tokens : Lexer.token array; pos : int }

let make tokens = { tokens; pos = 0 }

let current { tokens; pos } =
  if pos >= Array.length tokens then Lexer.EOF else tokens.(pos)

let advance p = { p with pos = p.pos + 1 }

let rec parse_primary p =
  match current p with
  | Lexer.TRUE -> (advance p, Expr.Literal (Expr.LitBool true))
  | Lexer.FALSE -> (advance p, Expr.Literal (Expr.LitBool false))
  | Lexer.NIL -> (advance p, Expr.Literal Expr.LitNil)
  | Lexer.NUMBER (n, _) -> (advance p, Expr.Literal (Expr.LitNum n))
  | Lexer.STRING s -> (advance p, Expr.Literal (Expr.LitStr s))
  | Lexer.LEFT_PAREN -> (
      let p', expr = parse_expression (advance p) in
      match current p' with
      | Lexer.RIGHT_PAREN -> (advance p', Expr.Grouping expr)
      | _ -> failwith "Expected ')' after expression")
  | _ -> failwith "Expected expression"

and parse_unary p =
  match current p with
  | Lexer.BANG ->
      let p', e = parse_unary (advance p) in
      (p', Expr.Unary (Expr.Not, e))
  | Lexer.MINUS ->
      let p', e = parse_unary (advance p) in
      (p', Expr.Unary (Expr.Negate, e))
  | _ -> parse_primary p (* no unary op — fall through to primary *)

(* factor handles * and / — higher precedence *)
and parse_factor p =
  let p', left = parse_unary p in
  let rec loop p left =
    match current p with
    | Lexer.STAR ->
        let p', right = parse_unary (advance p) in
        loop p' (Expr.Binary (left, Expr.Multiply, right))
    | Lexer.SLASH ->
        let p', right = parse_unary (advance p) in
        loop p' (Expr.Binary (left, Expr.Divide, right))
    | _ -> (p, left)
  in
  loop p' left

(* term handles + and - — lower precedence, calls factor for operands *)
and parse_term p =
  let p', left = parse_factor p in
  (* ← calls parse_factor, not parse_unary *)
  let rec loop p left =
    match current p with
    | Lexer.PLUS ->
        let p', right = parse_factor (advance p) in
        loop p' (Expr.Binary (left, Expr.Add, right))
    | Lexer.MINUS ->
        let p', right = parse_factor (advance p) in
        loop p' (Expr.Binary (left, Expr.Subtract, right))
    | _ -> (p, left)
  in
  loop p' left

and parse_comparison p =
  let p', left = parse_term p in
  let rec loop p left =
    match current p with
    | Lexer.GREATER ->
        let p', r = parse_term (advance p) in
        loop p' (Expr.Binary (left, Expr.GREATER, r))
    | Lexer.GREATER_EQUAL ->
        let p', r = parse_term (advance p) in
        loop p' (Expr.Binary (left, Expr.GREATER_EQUAL, r))
    | Lexer.LESS ->
        let p', r = parse_term (advance p) in
        loop p' (Expr.Binary (left, Expr.LESS, r))
    | Lexer.LESS_EQUAL ->
        let p', r = parse_term (advance p) in
        loop p' (Expr.Binary (left, Expr.LESS_EQUAL, r))
    | _ -> (p, left)
  in
  loop p' left

and parse_equality p =
  let p', left = parse_comparison p in
  let rec loop p left =
    match current p with
    | Lexer.EQUAL_EQUAL ->
        let p', r = parse_comparison (advance p) in
        loop p' (Expr.Binary (left, Expr.EQUAL, r))
    | Lexer.BANG_EQUAL ->
        let p', r = parse_comparison (advance p) in
        loop p' (Expr.Binary (left, Expr.NOT_EQUAL, r))
    | _ -> (p, left)
  in
  loop p' left

and parse_expression p = parse_equality p

let parse tokens =
  let _p', ast = parse_expression (make tokens) in
  ast

let parse tokens =
  let _p', ast = parse_expression (make tokens) in
  ast
