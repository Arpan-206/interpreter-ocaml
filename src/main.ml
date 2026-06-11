type token = LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE | EOF
type lexer = { input : string; pos : int }

let make input = { input; pos = 0 }

let current { input; pos } =
  if pos >= String.length input then '\x00' else input.[pos]

let advance l = { l with pos = l.pos + 1 }

let rec next_token l =
  match current l with
  | ' ' | '\t' | '\n' | '\r' -> next_token (advance l)
  | '(' -> (advance l, LEFT_PAREN, "(")
  | ')' -> (advance l, RIGHT_PAREN, ")")
  | '\x00' -> (l, EOF, "")
  | c ->
      Printf.eprintf "[line 1] Error: Unexpected character: %c\n" c;
      next_token (advance l)

let token_to_string tok lexeme =
  match tok with
  | LEFT_PAREN -> Printf.sprintf "LEFT_PAREN %s null" lexeme
  | RIGHT_PAREN -> Printf.sprintf "RIGHT_PAREN %s null" lexeme
  | EOF -> "EOF  null"

(* Returns true if a lex error occurred *)
let rec scan l had_error =
  let l', tok, lexeme = next_token l in
  print_endline (token_to_string tok lexeme);
  match tok with EOF -> had_error | _ -> scan l' had_error

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
