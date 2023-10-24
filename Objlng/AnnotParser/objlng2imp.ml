let tr_op: Objlng.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt


(* main translation function *)
let translate_program (p: Objlng.typ Objlng.program) =

  let rec all_var_in_class (curr_cla: 'a Objlng.class_def) (pstr: 'a Objlng.class_def list)= (match curr_cla.parent with
      |Some parent_name -> all_var_in_class(
        List.find (fun (sta:Objlng.typ Objlng.class_def) -> sta.name = parent_name) pstr
        ) pstr @ curr_cla.fields
      | None -> curr_cla.fields
      )
  in

  (* translation of an expression *)
  let rec tr_expr (te: Objlng.typ Objlng.expression): Imp.expression = match te.expr with
    | Cst n  -> Cst n
    | Bool b -> Bool b
    | Var x  -> Var x
    | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
    | Call(f, l) -> Call(f, List.map tr_expr l)
    | MCall(e, f, l) -> 
      let tr_e = tr_expr e in
      let tr_obj = if tr_e = Var "super" then Imp.Var "This" else tr_e in
      let tr_obj2, acc_class = match tr_e with
      | Var "super" -> Imp.Var "This", 2
      | _ -> tr_obj, 1
      in
      let tr_params = List.map tr_expr l in
      (* Compute the pointer to the method using the class descriptor offset (8 bytes) *)
      let place = (match e.annot with
      | TClass c -> 
        let c_cla = List.find (fun (x: 'a Objlng.class_def) -> x.name = c) p.classes in
        let rec f_place (method_list: 'a Objlng.function_def list) counter (f_cla:'a Objlng.class_def) counter2 = 
          (match method_list with
        | x::l when x.name = f -> 
           let rec obj_deref n = ( if n = 1 then Imp.Deref(tr_obj)
              else Imp.Deref(obj_deref (n-1))
           )in
           Imp.Binop(Add, obj_deref counter2, Cst (counter * 4))
        | x::l -> f_place l (counter + 1) f_cla counter2
        | _  ->  fc_place f_cla counter2
        ) and fc_place (f_cla:'a Objlng.class_def) counter = ( match f_cla.parent with
        | Some s -> let new_cla = List.find (fun (x: 'a Objlng.class_def) -> x.name = s) p.classes in 
          f_place new_cla.methods 1 new_cla (counter + 1)
        | None -> Printf.printf "Method %s not in the Classe %s \n " f c; failwith ""
        ) in
        (f_place c_cla.methods 1 c_cla acc_class) 
      | _ -> failwith "Method Call when the call is not a class"
        ) in
      let method_ptr = Imp.Deref(place) in
      (* Create the dynamic method call expression in the IMP abstract syntax *)
      DCall(method_ptr, tr_obj2 :: tr_params)
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
      let sum = (List.length (all_var_in_class s_classes pstr)) + 1 in
      Alloc(  Binop(Mul, Cst 4, Cst sum))
    | This ->  Var "This"
    | Super ->Var "super"
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
        aux s (all_var_in_class str_s pstr) 1
      in 
     Binop(Add, tr_expr e,  Cst (4 * place)) 
  in

  (* translation of instructions *)
  let rec tr_seq s = List.map tr_instr s
  and tr_instr: Objlng.typ Objlng.instruction -> Imp.instruction = function
    | Putchar e     -> Putchar(tr_expr e)
    | Set(s, e) -> (match e.expr with
      |(Objlng.New(s2, l)) -> Seq[
        Set(s, tr_expr e);
        Write(Var s, Var (s2^"_descr"));
        Expr(Call(s2^"_constructor", (Var s)::(List.map tr_expr l)))
      ]
      | _ -> Set(s, tr_expr e))
    | If(b, s1, s2) -> If(tr_expr b, tr_seq s1, tr_seq s2)
    | While(b, s) -> While(tr_expr b, tr_seq s)
    | Return e -> Return (tr_expr e)
    | Expr e -> Expr (tr_expr e)
    | Write (m, e) -> Write (tr_mem m, tr_expr e)
  in

  let init_fcla(fcla: Objlng.typ Objlng.class_def): Imp.instruction =
    let class_name = fcla.name ^"_descr" in
    let rec get_addr_fun(fdefc: Objlng.typ Objlng.function_def list) (acc:int) = 
      match fdefc with
      | x::l -> 
        (Imp.Write(Binop(Add, Var class_name, Cst acc), Addr (fcla.name^"_"^x.name)))::(get_addr_fun l (acc + 4))
      | _ -> []
    in
    let parent_name (fc: string option) = match fc with
    | Some s -> Imp.Var (s^"_descr")
    | None -> Cst 0
    in
    Imp.Seq([
      Imp.Set(class_name, Alloc(Cst ((List.length fcla.methods + 1) * 4)));
      Imp.Write(Var class_name,parent_name fcla.parent);
    ] @ get_addr_fun fcla.methods 4)
  in

  (* translation of function definitions *)
  let tr_fdef (fdef: Objlng.typ Objlng.function_def) =
    { Imp.name = fdef.name; 
      params = List.map fst fdef.params; 
      locals = List.map fst fdef.locals; 
      code = if fdef.name = "main" 
            then (List.map init_fcla p.classes) @ (tr_seq fdef.code)
            else tr_seq fdef.code;
    }
  in

  let tr_fcla (fcla: Objlng.typ Objlng.class_def) = 
    let tr_fdefc (fdefc: Objlng.typ Objlng.function_def): Imp.function_def =
      {
        Imp.name = fcla.name ^ "_" ^ fdefc.name;
        Imp.params = "This"::(List.map fst fdefc.params);
        Imp.locals = List.map fst fdefc.locals;
        code = tr_seq fdefc.code;
      }
    in
    List.map tr_fdefc fcla.methods
  in

  { Imp.globals = (List.map fst p.globals) 
    @ (List.map (fun (fcla: Objlng.typ Objlng.class_def):string -> fcla.name ^ "_descr" ) p.classes);
    functions = List.flatten (List.map tr_fcla p.classes) @ (List.map tr_fdef p.functions) }
