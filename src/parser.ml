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

and parse_expression p = parse_primary p

let parse tokens =
  let _p', ast = parse_expression (make tokens) in
  ast

let parse tokens =
  let _p', ast = parse_expression (make tokens) in
  ast
