(* value.ml — runtime value types for the Lox interpreter *)

type t =
  | VBool of bool
  | VNil
  | VNum of float
  | VString of string
  | VCallable of callable (* native function, e.g. clock() *)
  | VFun of
      callable (* user-defined function — same shape, printed differently *)

(* Both native and user functions share this record.
   The call field is a closure that captures the environment at definition time. *)
and callable = { arity : int; call : t list -> t; name : string }

(* Print a value to stdout in Lox format (no trailing newline) *)
let print = function
  | VBool true -> print_string "true"
  | VBool false -> print_string "false"
  | VNil -> print_string "nil"
  | VNum f ->
      (* Integer-valued floats print without decimal point: 1.0 → "1" *)
      if Float.is_integer f then print_string (string_of_int (int_of_float f))
      else print_string (string_of_float f)
  | VString s -> print_string s
  | VCallable c -> print_string ("<native fn " ^ c.name ^ ">")
  | VFun f -> print_string ("<fn " ^ f.name ^ ">")
