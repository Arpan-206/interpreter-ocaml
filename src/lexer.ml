type token =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | STAR
  | DOT
  | COMMA
  | PLUS
  | MINUS
  | SEMICOLON
  | SLASH
  | EQUAL
  | EQUAL_EQUAL
  | BANG
  | BANG_EQUAL
  | LESS
  | LESS_EQUAL
  | GREATER
  | GREATER_EQUAL
  | STRING of string
  | NUMBER of float * string
  | IDENTIFIER of string
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FOR
  | FUN
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF

type lexer = { input : string; pos : int; line : int }

let make input = { input; pos = 0; line = 1 }

let current { input; pos } =
  if pos >= String.length input then '\x00' else input.[pos]

let advance l = { l with pos = l.pos + 1 }
let peek l = current (advance l)

let read_number l =
  let buf = Buffer.create 8 in
  let rec consume l =
    match current l with
    | '0' .. '9' ->
        Buffer.add_char buf (current l);
        consume (advance l)
    | '.' when match peek l with '0' .. '9' -> true | _ -> false ->
        (* only consume '.' if followed by a digit — avoids eating method calls like 1.foo *)
        Buffer.add_char buf '.';
        consume (advance l)
    | _ -> l
  in
  let l' = consume l in
  let s = Buffer.contents buf in
  (l', float_of_string s, s)

let keyword_of_string = function
  | "and" -> Some AND
  | "class" -> Some CLASS
  | "else" -> Some ELSE
  | "false" -> Some FALSE
  | "for" -> Some FOR
  | "fun" -> Some FUN
  | "if" -> Some IF
  | "nil" -> Some NIL
  | "or" -> Some OR
  | "print" -> Some PRINT
  | "return" -> Some RETURN
  | "super" -> Some SUPER
  | "this" -> Some THIS
  | "true" -> Some TRUE
  | "var" -> Some VAR
  | "while" -> Some WHILE
  | _ -> None

type lex_result =
  | Token of token * string (* token + lexeme *)
  | LexError of string (* error message *)
  | Skip (* whitespace — just recurse *)

let next_token l =
  match current l with
  | ' ' | '\t' | '\r' -> (advance l, Skip)
  | '\n' -> ({ l with pos = l.pos + 1; line = l.line + 1 }, Skip)
  | '(' -> (advance l, Token (LEFT_PAREN, "("))
  | ')' -> (advance l, Token (RIGHT_PAREN, ")"))
  | '{' -> (advance l, Token (LEFT_BRACE, "{"))
  | '}' -> (advance l, Token (RIGHT_BRACE, "}"))
  | '*' -> (advance l, Token (STAR, "*"))
  | '.' -> (advance l, Token (DOT, "."))
  | ',' -> (advance l, Token (COMMA, ","))
  | '+' -> (advance l, Token (PLUS, "+"))
  | '-' -> (advance l, Token (MINUS, "-"))
  | ';' -> (advance l, Token (SEMICOLON, ";"))
  | '/' ->
      if peek l = '/' then
        let rec skip_line l =
          match current l with '\n' | '\x00' -> l | _ -> skip_line (advance l)
        in
        (skip_line (advance (advance l)), Skip)
      else (advance l, Token (SLASH, "/"))
  | '=' ->
      if peek l = '=' then (advance (advance l), Token (EQUAL_EQUAL, "=="))
      else (advance l, Token (EQUAL, "="))
  | '!' ->
      if peek l = '=' then (advance (advance l), Token (BANG_EQUAL, "!="))
      else (advance l, Token (BANG, "!"))
  | '<' ->
      if peek l = '=' then (advance (advance l), Token (LESS_EQUAL, "<="))
      else (advance l, Token (LESS, "<"))
  | '>' ->
      if peek l = '=' then (advance (advance l), Token (GREATER_EQUAL, ">="))
      else (advance l, Token (GREATER, ">"))
  | '"' ->
      let start_line = l.line in
      let buf = Buffer.create 32 in
      let rec scan_string l =
        match current l with
        | '\x00' ->
            ( l,
              LexError
                (Printf.sprintf "[line %d] Error: Unterminated string."
                   start_line) )
        | '"' ->
            let s = Buffer.contents buf in
            (advance l, Token (STRING s, Printf.sprintf "\"%s\"" s))
        | '\n' ->
            Buffer.add_char buf '\n';
            scan_string { l with pos = l.pos + 1; line = l.line + 1 }
        | c ->
            Buffer.add_char buf c;
            scan_string (advance l)
      in
      scan_string (advance l)
  | '\x00' -> (l, Token (EOF, ""))
  | '0' .. '9' ->
      let l', value, lexeme = read_number l in
      (l', Token (NUMBER (value, lexeme), lexeme))
  | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
      let buf = Buffer.create 8 in
      let rec consume l =
        match current l with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' ->
            Buffer.add_char buf (current l);
            consume (advance l)
        | _ -> l
      in
      let l' = consume l in
      let lexeme = Buffer.contents buf in
      let tok =
        match keyword_of_string lexeme with
        | Some kw -> kw
        | None -> IDENTIFIER lexeme
      in
      (l', Token (tok, lexeme))
  | c ->
      ( advance l,
        LexError
          (Printf.sprintf "[line %d] Error: Unexpected character: %c" l.line c)
      )

let token_to_string tok lexeme =
  match tok with
  | LEFT_PAREN -> Printf.sprintf "LEFT_PAREN %s null" lexeme
  | RIGHT_PAREN -> Printf.sprintf "RIGHT_PAREN %s null" lexeme
  | LEFT_BRACE -> Printf.sprintf "LEFT_BRACE %s null" lexeme
  | RIGHT_BRACE -> Printf.sprintf "RIGHT_BRACE %s null" lexeme
  | STAR -> Printf.sprintf "STAR %s null" lexeme
  | DOT -> Printf.sprintf "DOT %s null" lexeme
  | COMMA -> Printf.sprintf "COMMA %s null" lexeme
  | PLUS -> Printf.sprintf "PLUS %s null" lexeme
  | MINUS -> Printf.sprintf "MINUS %s null" lexeme
  | SEMICOLON -> Printf.sprintf "SEMICOLON %s null" lexeme
  | SLASH -> Printf.sprintf "SLASH %s null" lexeme
  | EQUAL -> Printf.sprintf "EQUAL %s null" lexeme
  | EQUAL_EQUAL -> Printf.sprintf "EQUAL_EQUAL %s null" lexeme
  | BANG -> Printf.sprintf "BANG %s null" lexeme
  | BANG_EQUAL -> Printf.sprintf "BANG_EQUAL %s null" lexeme
  | LESS -> Printf.sprintf "LESS %s null" lexeme
  | LESS_EQUAL -> Printf.sprintf "LESS_EQUAL %s null" lexeme
  | GREATER -> Printf.sprintf "GREATER %s null" lexeme
  | GREATER_EQUAL -> Printf.sprintf "GREATER_EQUAL %s null" lexeme
  | STRING s -> Printf.sprintf "STRING \"%s\" %s" s s
  | NUMBER (f, raw) ->
      let lit =
        if Float.is_integer f then Printf.sprintf "%.1f" f
        else string_of_float f
      in
      Printf.sprintf "NUMBER %s %s" raw lit
  | IDENTIFIER name -> Printf.sprintf "IDENTIFIER %s null" name
  | AND -> Printf.sprintf "AND %s null" lexeme
  | CLASS -> Printf.sprintf "CLASS %s null" lexeme
  | ELSE -> Printf.sprintf "ELSE %s null" lexeme
  | FALSE -> Printf.sprintf "FALSE %s null" lexeme
  | FOR -> Printf.sprintf "FOR %s null" lexeme
  | FUN -> Printf.sprintf "FUN %s null" lexeme
  | IF -> Printf.sprintf "IF %s null" lexeme
  | NIL -> Printf.sprintf "NIL %s null" lexeme
  | OR -> Printf.sprintf "OR %s null" lexeme
  | PRINT -> Printf.sprintf "PRINT %s null" lexeme
  | RETURN -> Printf.sprintf "RETURN %s null" lexeme
  | SUPER -> Printf.sprintf "SUPER %s null" lexeme
  | THIS -> Printf.sprintf "THIS %s null" lexeme
  | TRUE -> Printf.sprintf "TRUE %s null" lexeme
  | VAR -> Printf.sprintf "VAR %s null" lexeme
  | WHILE -> Printf.sprintf "WHILE %s null" lexeme
  | EOF -> "EOF  null"

let rec scan l had_error =
  let l', result = next_token l in
  match result with
  | Skip -> scan l' had_error
  | LexError msg ->
      Printf.eprintf "%s\n" msg;
      scan l' true
  | Token (EOF, _) ->
      print_endline "EOF  null";
      had_error
  | Token (tok, lexeme) ->
      print_endline (token_to_string tok lexeme);
      scan l' had_error

let tokenize input =
  let rec go l acc =
    let l', result = next_token l in
    match result with
    | Skip -> go l' acc
    | LexError msg ->
        Printf.eprintf "%s\n" msg;
        go l' acc
    | Token (tok, _) ->
        let acc' = (tok, l.line) :: acc in
        (* pair token with current line *)
        if tok = EOF then Array.of_list (List.rev acc') else go l' acc'
  in
  go (make input) []

let token_lexeme = function
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | STAR -> "*"
  | DOT -> "."
  | COMMA -> ","
  | PLUS -> "+"
  | MINUS -> "-"
  | SEMICOLON -> ";"
  | SLASH -> "/"
  | EQUAL -> "="
  | EQUAL_EQUAL -> "=="
  | BANG -> "!"
  | BANG_EQUAL -> "!="
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | AND -> "and"
  | CLASS -> "class"
  | ELSE -> "else"
  | FALSE -> "false"
  | FOR -> "for"
  | FUN -> "fun"
  | IF -> "if"
  | NIL -> "nil"
  | OR -> "or"
  | PRINT -> "print"
  | RETURN -> "return"
  | SUPER -> "super"
  | THIS -> "this"
  | TRUE -> "true"
  | VAR -> "var"
  | WHILE -> "while"
  | STRING s -> Printf.sprintf "\"%s\"" s
  | NUMBER (_, raw) -> raw
  | IDENTIFIER s -> s
  | EOF -> ""
