(* value.ml — runtime value types for the Lox interpreter *)

type t =
  | VBool of bool
  | VNil
  | VNum of float
  | VString of string
  | VCallable of callable
  | VFun of callable
  | VClass of lox_class
  | VInstance of lox_instance

and callable = { arity : int; call : t list -> t; name : string }

and lox_class = {
  class_name : string;
  methods : (string, int) Hashtbl.t;
  superclass : lox_class option;
}

and lox_instance = {
  instance_class : lox_class;
  mutable fields : (string * t) list;
}

let print = function
  | VBool true -> print_string "true"
  | VBool false -> print_string "false"
  | VNil -> print_string "nil"
  | VNum f ->
      if Float.is_integer f then print_string (string_of_int (int_of_float f))
      else print_string (string_of_float f)
  | VString s -> print_string s
  | VCallable c -> print_string ("<native fn " ^ c.name ^ ">")
  | VFun f -> print_string ("<fn " ^ f.name ^ ">")
  | VClass c -> print_string c.class_name
  | VInstance i -> Printf.printf "%s instance" i.instance_class.class_name
