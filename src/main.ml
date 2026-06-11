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
  | EOF

type lexer = { input : string; pos : int }

let make input = { input; pos = 0 }

let current { input; pos } =
  if pos >= String.length input then '\x00' else input.[pos]

let advance l = { l with pos = l.pos + 1 }
let peek l = current (advance l)

type lex_result =
  | Token of token * string (* token + lexeme *)
  | LexError of char (* bad character *)
  | Skip (* whitespace — just recurse *)

let next_token l =
  match current l with
  | ' ' | '\t' | '\n' | '\r' -> (advance l, Skip)
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
  | '/' -> (advance l, Token (SLASH, "/"))
  | '=' ->
      if peek l = '=' then (advance (advance l), Token (EQUAL_EQUAL, "=="))
      else (advance l, Token (EQUAL, "="))
  | '!' ->
      if peek l = '=' then (advance (advance l), Token (BANG_EQUAL, "!="))
      else (advance l, Token (BANG, "!"))
  | '\x00' -> (l, Token (EOF, ""))
  | c -> (advance l, LexError c)

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
  | EOF -> "EOF  null"

let rec scan l had_error =
  let l', result = next_token l in
  match result with
  | Skip -> scan l' had_error
  | LexError c ->
      Printf.eprintf "[line 1] Error: Unexpected character: %c\n" c;
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
