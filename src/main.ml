(* main.ml — entry point: routes CLI commands to the appropriate pipeline stage *)

let () =
  (* Require exactly two arguments: <command> and <filename> *)
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh <command> <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  (* Read the entire source file into a string *)
  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  match command with
  (* Lex only — print each token and exit 65 if any lex errors *)
  | "tokenize" ->
      let had_error = Lexer.scan (Lexer.make file_contents) false in
      if had_error then exit 65
  (* Lex + parse — print the AST of a single expression *)
  | "parse" ->
      let tokens = Lexer.tokenize file_contents in
      let ast = Parser.parse tokens in
      Expr.print ast;
      print_newline ()
  (* Lex + parse + evaluate a single expression, print the result *)
  | "evaluate" ->
      let tokens = Lexer.tokenize file_contents in
      let ast = Parser.parse tokens in
      let env = Env.make () in
      let value = Evaluator.eval env ast in
      Value.print value;
      print_newline ()
  (* Full interpreter — lex, parse a program, and execute all statements *)
  | "run" ->
      let tokens = Lexer.tokenize file_contents in
      let stmts = Parser.parse_program tokens in
      Evaluator.exec_program stmts
  | _ ->
      Printf.eprintf "Unknown command: %s\n" command;
      exit 1
