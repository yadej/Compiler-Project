open Objlng

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = unit function_def Env.t
type senv = unit class_def Env.t

(* utility function *)
let check (te: 'a Objlng.expression) (t: 'a): 'a Objlng.expression =
  if te.annot <> t then failwith "type error";
  te

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

let check_binop op = match op with
  | Objlng.Add | Objlng.Mul -> TInt
  | Objlng.Lt -> TBool 

let rec find_typ_struct s l = match l with
| (name, typ)::l2 when s = name -> typ
| (name, _)::l2 -> find_typ_struct s l2
| _ -> failwith "this variable is not in the structure"

(* main typing function *)
let type_program (p: unit program): typ program =

  
  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: unit function_def) -> f.name, f) p.functions) Env.empty in
  let senv = add2env (List.map (fun s -> s.name, s) p.classes) Env.empty in

  let get_var_class_inhe(scla: string) =
    let cla = Env.find scla senv in 
    let rec get_all_war(fcla: unit class_def) = match fcla.parent with
    | Some s -> get_all_war(Env.find s senv ) @ fcla.fields
    | None -> fcla.fields
    in
    get_all_war cla
  in
  let get_fun_class_inhe(scla: string) =
    let cla = Env.find scla senv in 
    let rec get_all_war(fcla: unit class_def) = match fcla.parent with
    | Some s -> get_all_war(Env.find s senv ) @ fcla.methods
    | None -> fcla.methods
    in
    get_all_war cla
  in

  (* typing a function definition *)
  let type_fdef (curr_class, tenv, fenv: string option * typ Env.t * unit function_def Env.t) 
  (fdef: unit function_def): typ function_def =
    (* add local elements to the environments *)
    let tenv = add2env fdef.params tenv in
    let tenv = add2env fdef.locals tenv in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)
    (* type expressions *)
    let rec type_expr (e: unit expression): typ expression = match e.expr with
      | Cst n -> mk_expr TInt (Cst n)
      | Bool b -> mk_expr TBool (Bool b)
      | Var x ->(try 
         mk_expr (Env.find x tenv) (Var x) 
          with Not_found ->(match curr_class with
          | Some s -> mk_expr (Env.find x (add2env (get_var_class_inhe s) Env.empty)) (Var x)
          | None -> failwith "Var pas dans une classe et non déclarer"
          ) 
          )
      | Binop (op, e1, e2) -> let nop = check_binop op in
         mk_expr (nop) (Binop(op, type_expr e1, type_expr e2 ))
      | Call(f, l) ->
        let func = (try Env.find f fenv  with Not_found -> Printf.printf "Cherche la fonction %s \n" f;  failwith "") in
         mk_expr func.return (Call(f, List.map type_expr l))
      | MCall(e, f, l) -> 
        let typ_e = type_expr e in
        let class_fun: unit class_def = (match typ_e.annot with
          | TClass(s) ->
          (try Env.find s senv  with Not_found -> Printf.printf "Cherche la method %s de la classe %ss\n" f s;  failwith "") 
          | _ -> failwith "Devrait chercher une classe")
          in
  
        let func = List.find (fun (x: unit function_def) -> x.name = f) (get_fun_class_inhe class_fun.name) in
        mk_expr func.return (MCall(typ_e ,f, List.map type_expr l))
      | New (s, l) ->
         mk_expr (TClass (Env.find s senv).name) (New (s, List.map type_expr l))
      | NewTab(t, s) ->
         mk_expr (TArray t) (NewTab(t,type_expr s))
      | Read m -> type_mem m
      | This -> (match curr_class with
        | Some s -> mk_expr (TClass s) This
        | None -> failwith "Should be in a class"
        )
      | Super ->( match curr_class with
      | Some s -> mk_expr (TClass s) Super
      | None -> failwith "Should be in a class"
      )
    and type_mem m = match m with
    | Arr(e1, e2) ->  let typ_e1 = type_expr e1 in
        let typ_e2 = type_expr e2 in
        (match typ_e1.annot with
        | TArray t -> mk_expr t ( Read (Arr(typ_e1, check typ_e2 TInt) ))
        | _ -> failwith "Should be an array"
        )
    | Atr(e, s) -> let typ_e = type_expr e in
      (match typ_e.annot with
      |TClass t -> 
        let curr_field = add2env (get_var_class_inhe t) Env.empty in
        mk_expr (
        Env.find s curr_field) ( Read (Atr(typ_e, s) ))
      | _ -> failwith "Should be a Class"
      )
    in
    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e     ->
        Putchar (check (type_expr e) TInt)
      | Set (s, e) -> 
        let typ_e =
           check (type_expr e) (Env.find s tenv) in
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
  let type_classes( fcla: unit class_def): typ class_def =
    let tenv = add2env (get_var_class_inhe fcla.name) tenv in
    let fenv = add2env (List.map (fun (f: unit function_def) -> f.name, f) (get_fun_class_inhe fcla.name)) fenv in
    let listSname = List.init (List.length fcla.methods) (fun (x: int) -> Some fcla.name, tenv, fenv) in
    {fcla with methods = List.map2 type_fdef listSname fcla.methods}
  in
  let listSname = List.init (List.length p.functions) (fun (x: int) -> None, tenv, fenv) in 
  {p with functions = List.map2 type_fdef listSname p.functions; classes = List.map type_classes p.classes}