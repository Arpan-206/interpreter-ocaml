open Core_bench

let src = Stdlib.String.concat "\n" (List.init 1000 (fun _ -> "print 1 + 2;"))

let () =
  Command_unix.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"lex" (fun () -> ignore (Lexer.tokenize src));
         Bench.Test.create ~name:"lex+parse" (fun () ->
             let tokens = Lexer.tokenize src in
             ignore (Parser.parse_program tokens));
         Bench.Test.create ~name:"full run" (fun () ->
             let tokens = Lexer.tokenize src in
             let stmts = Parser.parse_program tokens in
             ignore (Evaluator.exec_program stmts));
       ])
