let tr_op: Objlng.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt


(* main translation function *)
let translate_program (p: Objlng.typ Objlng.program) =

  (* translation of an expression *)
  let rec tr_expr (te: Objlng.typ Objlng.expression): Imp.expression = match te.expr with
    | Cst n  -> Cst n
    | Bool b -> Bool b
    | Var x  -> Var x
    | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
    | Call(f, l) -> Call(f, List.map tr_expr l)
    | MCall(e, f, l) -> 
      let tr_obj = tr_expr e in
      let tr_params = List.map tr_expr l in
      (* Compute the pointer to the method using the class descriptor offset (8 bytes) *)
      let place = (match e.annot with
      | TClass c ->
        let c_cla = List.find (fun (x: 'a Objlng.class_def) -> x.name = c) p.classes in
        let rec f_place (method_list: 'a Objlng.function_def list) counter = (match method_list with
        | x::l when x.name = f -> counter
        | x::l -> f_place l (counter + 1)
        | _  -> Printf.printf "Method %s not in the Classe %s \n " f c; failwith ""
        ) in
        (List.length c_cla.fields) + (f_place c_cla.methods 0) 
      | _ -> failwith "Method Call when the call is not a class"
        ) in
      let method_ptr = Imp.Deref(Binop(Add, Deref(tr_obj), Cst (place * 4))) in
      (* Create the dynamic method call expression in the IMP abstract syntax *)
      DCall(method_ptr, tr_obj :: tr_params)
    | Read m -> Deref(tr_mem m)
    | NewTab(t, e) ->  let size = tr_expr e in 
    Alloc( Binop(Mul, Cst 4, size))
    | New (s, l) -> let pstr = p.classes in      
      let s_classes = 
        let rec aux (st:string) (pst:'a Objlng.class_def list) = (match pst with
        | x::l when x.name = st -> x
        | x::l -> aux s l
        | _ -> failwith "Name not in the struct")
        in
        aux s pstr
      in
      let sum = (List.length s_classes.fields) + (List.length s_classes.methods) in
      Alloc(  Binop(Mul, Cst 4, Cst sum))
    | This -> Cst 0
  and tr_mem m = match m with
  | Arr(e1, e2) -> let tr_e1 = tr_expr e1 in
      let tr_e2 = tr_expr e2 in
      Binop(Add, tr_e1, Binop(Mul, Cst 4, tr_e2)) 
  | Atr(e, s) ->
      let pstr = p.classes in 
      let str_e = (match e.annot with
      | TClass t -> t
      | _ -> failwith "This variable should be a  Structure"
      ) in
      let str_s = List.find (fun (x:'a Objlng.class_def) -> x.name = str_e) pstr in
      let place = 
        let rec aux (st:string) (pst:(string * Objlng.typ) list ) (counter:int) = match pst with
        | (x, _)::l when String.compare x st = 0 -> counter
        | (x, _)::l -> aux s l (counter+1)
        | _ -> failwith "Name not in the struct"
        in
        (** The List.find is to search the correct structure in the program *)
        aux s str_s.fields 0
      in 
     Binop(Add, tr_expr e, Binop(Mul, Cst 4, Cst place)) 
  in

  (* translation of instructions *)
  let rec tr_seq s = List.map tr_instr s
  and tr_instr: Objlng.typ Objlng.instruction -> Imp.instruction = function
    | Putchar e     -> Putchar(tr_expr e)
    | Set(s, e) -> Set(s, tr_expr e)
    | If(b, s1, s2) -> If(tr_expr b, tr_seq s1, tr_seq s2)
    | While(b, s) -> While(tr_expr b, tr_seq s)
    | Return e -> Return (tr_expr e)
    | Expr e -> Expr (tr_expr e)
    | Write (m, e) -> Write (tr_mem m, tr_expr e)
  in

  (* translation of function definitions *)
  let tr_fdef (fdef: Objlng.typ Objlng.function_def) =
    { Imp.name = fdef.name; 
      params = List.map fst fdef.params; 
      locals = List.map fst fdef.locals; 
      code = tr_seq fdef.code;
    }
  in

  { Imp.globals = List.map fst p.globals;
    functions = List.map tr_fdef p.functions }
