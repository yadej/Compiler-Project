(**
   Abstract syntax for the IMP language.
   Definition of types and data structures to represent an IMP program
   as caml data.
 *)

(**
   Binary operators: +, *, <
 *)
type binop = Add | Mul | Lt

(**
   Data structure for expressions
 *)
type expression =
  (* Integer constant: 0, -1, 42, ... *)
  | Cst   of int
  (* Boolean constant: true, false *)
  | Bool  of bool
  (* Variable, identified by a name *)
  | Var   of string
  (* Binary operation, with an operator and two operands *)
  | Binop of binop * expression * expression
  (* Function call, with a function name and a list of parameters *)
  | Call  of string * expression list
  (* Method call, with the Method place and a list of parameters*)
  | DCall of expression * expression list
  (* Dereference a pointer *)
  | Deref of expression   (*   *e   *)
  (* Allocate some memory *)
  | Alloc of expression
  (* Adress of Var *)
  | Addr of string

let array_offset t i = Binop(Add, t, Binop(Mul, Cst 4, i))
let array_access t i = Deref(array_offset t i)

(**
   Data structure for instructions
*)
type instruction =
  (* Primitive operation for printing a char, given as ASCII code *)
  | Putchar of expression
  (* Assignment of a new value to a variable *)
  | Set     of string * expression
  (* Conditional *)
  | If      of expression * sequence * sequence
  (* Loop *)
  | While   of expression * sequence
  (* Function termination *)
  | Return  of expression
  (* Expression used as an instruction (typically function call) *)
  | Expr    of expression
  (* writing in memory *)
  | Write   of expression * expression (*   *e1 = e2;   *)
  | Seq of sequence
(* Instruction sequence *)
and sequence = instruction list

let array_write t i e =
  Write(array_offset t i, e)

(** 
   Data structure for a function definition
 *)
type function_def = {
    (* Function name *)
    name: string;
    (* List of named parameters *)
    params: string list;
    (* List of named local variables *)
    locals: string list;
    (* The actual code *)
    code: sequence;
  }

(**
   Data structure for a program
 *)
type program = {
    (* List of named global variables *)
    globals: string list;
    (* The functions defined by the program *)
    functions: function_def list;
  }
