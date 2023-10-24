(**
   Annotated abstract syntax for the OBJEling language.
 *)

(* Types of SIMP values *)
type typ =
  | TInt 
  | TBool
  | TClass of string (* class type, identified by its name *)
  | TArray of typ    (* array containing elements of the specified type *)
  | TVoid (* not an actual type in the source language, but having it in
             the AST makes the code more uniform *)

type binop = Add | Mul | Lt

type 'a expression = {
  annot: 'a;
  expr: 'a expr;
}
and 'a expr =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of binop * 'a expression * 'a expression
  | Call  of string * 'a expression list
  | MCall of 'a expression * string * 'a expression list
  | New   of string * 'a expression list (* create an instance and call the constructor *)
  | NewTab of typ * 'a expression (* create an array of the given type and size *)
  | Read  of 'a mem               (* read in memory *)
  | This (* current object *)
  | Super (* parent object *)
and 'a mem =
  | Arr of 'a expression * 'a expression (* array access     e1[e2]  *)
  | Atr of 'a expression * string        (* attribute access  o.x    *)

let mk_expr a e = { annot=a; expr=e }

let mk_expr_to_mem (e: 'a expression): (typ mem) = match e.expr with
| Read x -> x
| _ -> failwith " Should be a Read to make an expr to Mem "

type 'a instruction =
  | Putchar of 'a expression
  | Set     of string * 'a expression
  | If      of 'a expression * 'a sequence * 'a sequence
  | While   of 'a expression * 'a sequence
  | Return  of 'a expression
  | Expr    of 'a expression
  | Write   of 'a mem * 'a expression (*   m = e;   *)
and 'a sequence = 'a instruction list

(* Function definition *)
type 'a function_def = {
  name:   string;
  params: (string * typ) list;
  locals: (string * typ) list;
  code:   'a sequence;
  return: typ;
}

(* Class definition *)
type 'a class_def = {
  name:   string;
  fields: (string * typ) list;
  methods: 'a function_def list;
  parent: string option;
}

(* Program as in IMP + types + user-defined  *)
type 'a program = {
  globals:   (string * typ) list;
  functions: 'a function_def list;
  classes:   'a class_def list;
}
