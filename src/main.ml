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
  | EOF

type lexer = { input : string; pos : int; line : int }

let make input = { input; pos = 0; line = 1 }

let current { input; pos } =
  if pos >= String.length input then '\x00' else input.[pos]

let advance l = { l with pos = l.pos + 1 }
let peek l = current (advance l)

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
      let rec scan_string l acc =
        match current l with
        | '\x00' ->
            ( l,
              LexError
                (Printf.sprintf "[line %d] Error: Unterminated string."
                   start_line) )
        | '"' -> (advance l, Token (STRING acc, Printf.sprintf "\"%s\"" acc))
        | '\n' ->
            scan_string
              { l with pos = l.pos + 1; line = l.line + 1 }
              (acc ^ "\n")
        | c -> scan_string (advance l) (acc ^ String.make 1 c)
      in
      scan_string (advance l) ""
  | '\x00' -> (l, Token (EOF, ""))
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
  | STRING s -> Printf.sprintf "STRING %s null" s
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

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  let had_error = scan (make file_contents) false in
  if had_error then exit 65
