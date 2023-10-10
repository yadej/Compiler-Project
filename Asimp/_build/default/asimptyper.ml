open Asimp

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = unit function_def Env.t
type senv = struct_def Env.t

(* utility function *)
let check (te: 'a Asimp.expression) (t: 'a): 'a Asimp.expression =
  if te.annot <> t then failwith "type error";
  te

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

let check_binop op = match op with
  | Asimp.Add | Asimp.Mul -> TInt
  | Asimp.Lt -> TBool 

let rec find_typ_struct s l = match l with
| (name, typ)::l2 when s = name -> typ
| (name, _)::l2 -> find_typ_struct s l2
| _ -> failwith "this variable is not is the structure"

(* main typing function *)
let type_program (p: unit program): typ program =

  
  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: unit function_def) -> f.name, f) p.functions) Env.empty in
  let senv = add2env (List.map (fun s -> s.name, s) p.structs) Env.empty in

  (* typing a function definition *)
  let type_fdef fdef =
    (* add local elements to the environments *)
    let tenv = add2env fdef.params tenv in
    let tenv = add2env fdef.locals tenv in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)
    (* type expressions *)
    let rec type_expr (e: unit expression): typ expression = match e.expr with
      | Cst n -> mk_expr TInt (Cst n)
      | Bool b -> mk_expr TBool (Bool b)
      | Var x ->( try 
         mk_expr (Env.find x tenv) (Var x) with Not_found -> Printf.printf "Cherche %s\n" x;failwith "Should be a variable" )
      | Binop (op, e1, e2) -> let nop = check_binop op in
         mk_expr (nop) (Binop(op, type_expr e1, type_expr e2 ))
      | Call(f, l) ->
         let func = Env.find f fenv in
         mk_expr func.return (Call(f, List.map type_expr l))
      | New s -> mk_expr (TStruct (Env.find s senv).name) (New s)
      | NewTab(t, s) ->
         mk_expr (TArray t) (NewTab(t,type_expr s))
      | Read m -> type_mem m
    and type_mem m = match m with
    | Arr(e1, e2) ->  let typ_e1 = type_expr e1 in
        let typ_e2 = type_expr e2 in
        (match typ_e1.annot with
        | TArray t -> mk_expr t ( Read (Arr(typ_e1, check typ_e2 TInt) ))
        | _ -> failwith "Should be an array"
        )
    | Str(e, s) -> let typ_e = type_expr e in
      (match typ_e.annot with
      |TStruct t -> 
        mk_expr (
        find_typ_struct s (Env.find t senv).fields ) ( Read (Str(typ_e, s) ))
      | _ -> failwith "Should be a Struct"
      )
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e     -> Putchar (check (type_expr e) TInt)
      | Set (s, e) -> let typ_e = check (type_expr e) (Env.find s tenv) in
        Set (s ,typ_e)
      | If(b, s1, s2) -> If(check (type_expr b) TBool, type_seq s1, type_seq s2)
      | While(b, s) -> While(check (type_expr b) TBool, type_seq s )
      | Return e -> Return (type_expr e)
      | Expr e -> Expr (type_expr e)
      | Write(m, e) ->
          Write( mk_expr_to_mem (type_mem m), type_expr e)
    in
    { fdef with code = type_seq fdef.code }
  in
  { p with functions = List.map type_fdef p.functions }
