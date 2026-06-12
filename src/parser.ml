(* parser.ml — recursive descent parser for Lox
   Produces an Expr.t (single expression) or Stmt.t list (full program).

   Precedence ladder (lowest → highest):
     expression → assignment → or → and → equality → comparison
     → term → factor → unary → call → primary
*)

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

(* ── Expression parsing ─────────────────────────────────────────────────── *)

let rec parse_primary p =
  match current_tok p with
  | Lexer.TRUE -> (advance p, Expr.Literal (Expr.LitBool true))
  | Lexer.FALSE -> (advance p, Expr.Literal (Expr.LitBool false))
  | Lexer.NIL -> (advance p, Expr.Literal Expr.LitNil)
  | Lexer.NUMBER (n, _) -> (advance p, Expr.Literal (Expr.LitNum n))
  | Lexer.STRING s -> (advance p, Expr.Literal (Expr.LitStr s))
  | Lexer.IDENTIFIER name ->
      let line = current_line p in
      (advance p, Expr.Variable (name, line, Expr.fresh_id ()))
  | Lexer.LEFT_PAREN -> (
      let p', expr = parse_expression (advance p) in
      match current_tok p' with
      | Lexer.RIGHT_PAREN -> (advance p', Expr.Grouping expr)
      | _ -> parse_error p' "Expect ')' after expression.")
  | _ -> parse_error p "Expect expression."

(* Function call — left-associative suffix after primary *)
and parse_call p =
  let p, expr = parse_primary p in
  let rec loop p expr =
    match current_tok p with
    | Lexer.LEFT_PAREN ->
        let p = advance p in
        let rec parse_args p acc =
          match current_tok p with
          | Lexer.RIGHT_PAREN -> (advance p, List.rev acc)
          | _ -> (
              let p, arg = parse_expression p in
              let acc = arg :: acc in
              match current_tok p with
              | Lexer.COMMA -> parse_args (advance p) acc
              | Lexer.RIGHT_PAREN -> (advance p, List.rev acc)
              | _ -> parse_error p "Expect ')' after arguments.")
        in
        let line = current_line p in
        let p, args = parse_args p [] in
        loop p (Expr.Call (expr, args, line))
    | _ -> (p, expr)
  in
  loop p expr

and parse_unary p =
  match current_tok p with
  | Lexer.BANG ->
      let p', e = parse_unary (advance p) in
      (p', Expr.Unary (Expr.Not, e))
  | Lexer.MINUS ->
      let p', e = parse_unary (advance p) in
      (p', Expr.Unary (Expr.Negate, e))
  | _ -> parse_call p

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

and parse_and p =
  let p, left = parse_equality p in
  let rec loop p left =
    match current_tok p with
    | Lexer.AND ->
        let p, right = parse_equality (advance p) in
        loop p (Expr.And (left, right))
    | _ -> (p, left)
  in
  loop p left

and parse_or p =
  let p, left = parse_and p in
  let rec loop p left =
    match current_tok p with
    | Lexer.OR ->
        let p, right = parse_and (advance p) in
        loop p (Expr.Or (left, right))
    | _ -> (p, left)
  in
  loop p left

(* Assignment — right-associative, target must be a variable. *)
and parse_assignment p =
  let p', expr = parse_or p in
  match current_tok p' with
  | Lexer.EQUAL -> (
      let p'', value = parse_assignment (advance p') in
      match expr with
      | Expr.Variable (name, line, _) ->
          (p'', Expr.Assign (name, value, line, Expr.fresh_id ()))
      | _ -> parse_error p' "Invalid assignment target.")
  | _ -> (p', expr)

and parse_expression p = parse_assignment p

(* ── Statement parsing ──────────────────────────────────────────────────── *)

and parse_declaration p =
  match current_tok p with
  | Lexer.VAR -> (
      let line = current_line p in
      let p' = advance p in
      match current_tok p' with
      | Lexer.IDENTIFIER name -> (
          let p'' = advance p' in
          match current_tok p'' with
          | Lexer.EQUAL ->
              let p''', init = parse_expression (advance p'') in
              let p''' =
                match current_tok p''' with
                | Lexer.SEMICOLON -> advance p'''
                | _ -> parse_error p''' "Expect ';' after variable declaration."
              in
              (p''', Stmt.VarDecl (name, Some init, line))
          | Lexer.SEMICOLON -> (advance p'', Stmt.VarDecl (name, None, line))
          | _ -> parse_error p'' "Expect '=' or ';' after variable name.")
      | _ -> parse_error p' "Expect variable name.")
  | Lexer.FUN -> (
      let line = current_line p in
      let p = advance p in
      match current_tok p with
      | Lexer.IDENTIFIER name ->
          let p = advance p in
          let p =
            match current_tok p with
            | Lexer.LEFT_PAREN -> advance p
            | _ -> parse_error p "Expect '(' after function name."
          in
          let rec parse_params p acc =
            match current_tok p with
            | Lexer.RIGHT_PAREN -> (advance p, List.rev acc)
            | Lexer.IDENTIFIER param -> (
                let p = advance p in
                let acc = param :: acc in
                match current_tok p with
                | Lexer.COMMA -> parse_params (advance p) acc
                | Lexer.RIGHT_PAREN -> (advance p, List.rev acc)
                | _ -> parse_error p "Expect ')' after parameters.")
            | _ -> parse_error p "Expect parameter name."
          in
          let p, params = parse_params p [] in
          let p =
            match current_tok p with
            | Lexer.LEFT_BRACE -> advance p
            | _ -> parse_error p "Expect '{' before function body."
          in
          let rec parse_body p acc =
            match current_tok p with
            | Lexer.RIGHT_BRACE -> (advance p, List.rev acc)
            | Lexer.EOF -> parse_error p "Expect '}' after block."
            | _ ->
                let p, s = parse_declaration p in
                parse_body p (s :: acc)
          in
          let p, body = parse_body p [] in
          (p, Stmt.FunDecl (name, params, body, line))
      | _ -> parse_error p "Expect function name.")
  | _ -> parse_statement p

and parse_statement p =
  match current_tok p with
  | Lexer.PRINT -> (
      let p' = advance p in
      let p'', expr = parse_expression p' in
      match current_tok p'' with
      | Lexer.SEMICOLON -> (advance p'', Stmt.Print expr)
      | _ -> parse_error p'' "Expect ';' after value.")
  | Lexer.RETURN ->
      let p = advance p in
      let p, value =
        match current_tok p with
        | Lexer.SEMICOLON -> (advance p, None)
        | _ ->
            let p, e = parse_expression p in
            let p =
              match current_tok p with
              | Lexer.SEMICOLON -> advance p
              | _ -> parse_error p "Expect ';' after return value."
            in
            (p, Some e)
      in
      (p, Stmt.Return value)
  | Lexer.LEFT_BRACE ->
      let rec parse_block p acc =
        match current_tok p with
        | Lexer.RIGHT_BRACE -> (advance p, Stmt.Block (List.rev acc))
        | Lexer.EOF -> parse_error p "Expect '}' after block."
        | _ ->
            let p', stmt = parse_declaration p in
            parse_block p' (stmt :: acc)
      in
      parse_block (advance p) []
  | Lexer.IF ->
      let p = advance p in
      let p =
        match current_tok p with
        | Lexer.LEFT_PAREN -> advance p
        | _ -> parse_error p "Expect '(' after 'if'."
      in
      let p, condition = parse_expression p in
      let p =
        match current_tok p with
        | Lexer.RIGHT_PAREN -> advance p
        | _ -> parse_error p "Expect ')' after if condition."
      in
      let p, then_branch = parse_statement p in
      let p, else_branch =
        match current_tok p with
        | Lexer.ELSE ->
            let p, s = parse_statement (advance p) in
            (p, Some s)
        | _ -> (p, None)
      in
      (p, Stmt.If (condition, then_branch, else_branch))
  | Lexer.WHILE ->
      let p = advance p in
      let p =
        match current_tok p with
        | Lexer.LEFT_PAREN -> advance p
        | _ -> parse_error p "Expect '(' after 'while'."
      in
      let p, condition = parse_expression p in
      let p =
        match current_tok p with
        | Lexer.RIGHT_PAREN -> advance p
        | _ -> parse_error p "Expect ')' after while condition."
      in
      let p, body = parse_statement p in
      (p, Stmt.While (condition, body))
  | Lexer.CLASS -> (
      let line = current_line p in
      let p = advance p in
      match current_tok p with
      | Lexer.IDENTIFIER name ->
          let p = advance p in
          let p =
            match current_tok p with
            | Lexer.LEFT_BRACE -> advance p
            | _ -> parse_error p "Expect '{' before class body."
          in
          (* For now: skip over the empty body *)
          let p =
            match current_tok p with
            | Lexer.RIGHT_BRACE -> advance p
            | _ -> parse_error p "Expect '}' after class body."
          in
          (p, Stmt.ClassDecl (name, line))
      | _ -> parse_error p "Expect class name.")
  | Lexer.FOR ->
      (* Desugared into: Block [init; While (cond) Block [body; incr]] *)
      let p = advance p in
      let p =
        match current_tok p with
        | Lexer.LEFT_PAREN -> advance p
        | _ -> parse_error p "Expect '(' after 'for'."
      in
      let p, init =
        match current_tok p with
        | Lexer.SEMICOLON -> (advance p, None)
        | Lexer.VAR ->
            let p, s = parse_declaration p in
            (p, Some s)
        | _ ->
            let p, e = parse_expression p in
            let p =
              match current_tok p with
              | Lexer.SEMICOLON -> advance p
              | _ -> parse_error p "Expect ';' after for initializer."
            in
            (p, Some (Stmt.Expression e))
      in
      let p, condition =
        match current_tok p with
        | Lexer.SEMICOLON -> (advance p, None)
        | _ ->
            let p, e = parse_expression p in
            let p =
              match current_tok p with
              | Lexer.SEMICOLON -> advance p
              | _ -> parse_error p "Expect ';' after loop condition."
            in
            (p, Some e)
      in
      let p, increment =
        match current_tok p with
        | Lexer.RIGHT_PAREN -> (advance p, None)
        | _ ->
            let p, e = parse_expression p in
            let p =
              match current_tok p with
              | Lexer.RIGHT_PAREN -> advance p
              | _ -> parse_error p "Expect ')' after for clauses."
            in
            (p, Some e)
      in
      let p, body = parse_statement p in
      let body =
        match increment with
        | Some e -> Stmt.Block [ body; Stmt.Expression e ]
        | None -> body
      in
      let cond =
        match condition with
        | Some e -> e
        | None -> Expr.Literal (Expr.LitBool true)
      in
      let body = Stmt.While (cond, body) in
      let body =
        match init with Some s -> Stmt.Block [ s; body ] | None -> body
      in
      (p, body)
  | _ -> (
      let p', expr = parse_expression p in
      match current_tok p' with
      | Lexer.SEMICOLON -> (advance p', Stmt.Expression expr)
      | _ -> parse_error p' "Expect ';' after expression.")

(* ── Top-level program parsing ──────────────────────────────────────────── *)

let rec parse_program' p acc =
  match current_tok p with
  | Lexer.EOF -> List.rev acc
  | _ ->
      let p', stmt = parse_declaration p in
      parse_program' p' (stmt :: acc)

let parse_program tokens = parse_program' (make tokens) []

let parse tokens =
  let _p', ast = parse_expression (make tokens) in
  ast
