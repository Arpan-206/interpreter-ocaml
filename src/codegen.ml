(* codegen.ml — drives the LLVM AOT compiler via FFI *)

external lox_compile : bytes -> int -> string -> int = "lox_compile"

let compile stmts output_path =
  let bytes = Serialise.serialise stmts in
  let rc =
    lox_compile (Bytes.of_string bytes) (String.length bytes) output_path
  in
  if rc <> 0 then (
    Printf.eprintf "AOT compilation failed\n";
    exit 1)
