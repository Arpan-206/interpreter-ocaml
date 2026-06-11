type parser = { tokens : (Lexer.token * int) array; pos : int }

let make tokens = { tokens; pos = 0 }

let current { tokens; pos } =
  if pos >= Array.length tokens then (Lexer.EOF, 0) else tokens.(pos)

let current_tok p = fst (current p)
let current_line p = snd (current p)
let advance p = { p with pos = p.pos + 1 }

let parse_error p msg =
  let line = current_line p in
  let location =
    match current_tok p with
    | Lexer.EOF -> "end"
    | tok -> Printf.sprintf "'%s'" (Lexer.token_lexeme tok)
  in
  Printf.eprintf "[line %d] Error at %s: %s\n" line location msg;
  exit 65

let rec parse_primary p =
  match current_tok p with
  | Lexer.TRUE -> (advance p, Expr.Literal (Expr.LitBool true))
  | Lexer.FALSE -> (advance p, Expr.Literal (Expr.LitBool false))
  | Lexer.NIL -> (advance p, Expr.Literal Expr.LitNil)
  | Lexer.NUMBER (n, _) -> (advance p, Expr.Literal (Expr.LitNum n))
  | Lexer.STRING s -> (advance p, Expr.Literal (Expr.LitStr s))
  | Lexer.LEFT_PAREN -> (
      let p', expr = parse_expression (advance p) in
      match current_tok p' with
      | Lexer.RIGHT_PAREN -> (advance p', Expr.Grouping expr)
      | _ -> parse_error p' "Expect ')' after expression.")
  | _ -> parse_error p "Expect expression."

and parse_unary p =
  match current_tok p with
  | Lexer.BANG ->
      let p', e = parse_unary (advance p) in
      (p', Expr.Unary (Expr.Not, e))
  | Lexer.MINUS ->
      let p', e = parse_unary (advance p) in
      (p', Expr.Unary (Expr.Negate, e))
  | _ -> parse_primary p

and parse_factor p =
  let p', left = parse_unary p in
  let rec loop p left =
    match current_tok p with
    | Lexer.STAR ->
        let p', r = parse_unary (advance p) in
        loop p' (Expr.Binary (left, Expr.Multiply, r))
    | Lexer.SLASH ->
        let p', r = parse_unary (advance p) in
        loop p' (Expr.Binary (left, Expr.Divide, r))
    | _ -> (p, left)
  in
  loop p' left

and parse_term p =
  let p', left = parse_factor p in
  let rec loop p left =
    match current_tok p with
    | Lexer.PLUS ->
        let p', r = parse_factor (advance p) in
        loop p' (Expr.Binary (left, Expr.Add, r))
    | Lexer.MINUS ->
        let p', r = parse_factor (advance p) in
        loop p' (Expr.Binary (left, Expr.Subtract, r))
    | _ -> (p, left)
  in
  loop p' left

and parse_comparison p =
  let p', left = parse_term p in
  let rec loop p left =
    match current_tok p with
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
    match current_tok p with
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

let parse_statement p =
  match current_tok p with
  | Lexer.PRINT -> (
      let p' = advance p in
      let p'', expr = parse_expression p' in
      match current_tok p'' with
      | Lexer.SEMICOLON -> (advance p'', Stmt.Print expr)
      | _ -> parse_error p'' "Expect ';' after value.")
  | _ -> (
      let p', expr = parse_expression p in
      match current_tok p' with
      | Lexer.SEMICOLON -> (advance p', Stmt.Expression expr)
      | _ -> parse_error p' "Expect ';' after expression.")

let rec parse_program' p acc =
  match current_tok p with
  | Lexer.EOF -> List.rev acc
  | _ ->
      let p', stmt = parse_statement p in
      parse_program' p' (stmt :: acc)

let parse_program tokens = parse_program' (make tokens) []

let parse tokens =
  let _p', ast = parse_expression (make tokens) in
  ast
