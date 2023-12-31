(**
   Translation from IMP to MIPS.

   Result of an expression stored in $t0. Every intermediate value on the
   stack, every function argument and every local variable also on the stack.
 *)

open Imp
open Mips

exception Error of string

let tmp_regs = [| t0; t1; t2; t3; t4; t5; t6; t7; t8; t9 |]
let nb_tmp_regs = Array.length tmp_regs

let var_regs = [| s0; s1; s2; s3; s4; s5; s6; s7 |]
let nb_var_regs = Array.length var_regs

let a_regs = [|a0; a1; a2; a3|]

let push reg = subi sp sp 4 @@ sw reg 0(sp)
let pop  reg = lw reg 0(sp) @@ addi sp sp 4

let rec    save regs k = if k < 0 then nop else save regs (k-1) @@ push regs.(k)
let rec restore regs k = if k < 0 then nop else    pop regs.(k) @@ restore regs (k-1)

let    save_tmp = save    tmp_regs
let restore_tmp = restore tmp_regs

let save_a_reg = save a_regs
let restore_a_reg = restore a_regs

let rec simplify_expr = function
| Cst n -> Cst n
| Bool b -> Bool b
| Binop (Add,Cst a,Cst b) -> Cst (a + b)
| Binop (Mul,Cst a,Cst b) -> Cst (a * b)
| Binop (Lt,Cst a,Cst b) -> Bool (a < b)
| Binop (op, a, b) -> 
    let fun_a = simplify_expr a in
    let fun_b = simplify_expr b in
    (
    match (fun_a,fun_b) with
    | Cst _,Cst _|Bool _, Bool _ -> simplify_expr (Binop(op, fun_a,  fun_b))
    | Cst a, Binop(nop ,Cst b, x) | Binop(nop ,Cst b, x) ,Cst a
    | Cst a, Binop(nop ,x ,Cst b) | Binop(nop ,x ,Cst b) ,Cst a
    when op = nop && nop != Lt -> 
      let operator = if op = Add then (fun x y -> x + y)
      else (fun x y -> x * y)
     in 
      Binop(op,Cst (operator a b),simplify_expr x)
    | _ , _ -> Binop(op, fun_a,  fun_b)
    )
| Var x -> Var x
| Call(s, l) -> Call(s, List.map simplify_expr l)

(* explicit allocation information for a local variable *)
type explicit_alloc =
  | Reg   of string  (* name of the register *)
  | Stack of int     (* offset on the stack, relative to fp *)

(* create an explicit allocation for all local variables and function 
   parameters of a function [fdef] *)
let allocate_locals fdef =
  let nfdef = Nimp.from_imp_fdef fdef in
  let raw_alloc, r_max, spill_count = Linearscan.lscan_alloc nb_var_regs nfdef in
  let new_spill_count = ref spill_count in
  let alloc = Hashtbl.create 32 in
  List.iter(fun  var -> 
    let raw = Hashtbl.find raw_alloc var in
    let explicit =  try
      match raw with
      | Linearscan.RegN n -> Reg(var_regs.(n))
      | Linearscan.Spill offset ->  Stack(-4 * offset - 8)
      with Not_found -> new_spill_count := !new_spill_count + 1;
        Stack(-4 * !new_spill_count - 8)
      in
      Hashtbl.add alloc var explicit
  )  (fdef.params @ fdef.locals); 
  alloc, r_max

(* Generate Mips code for an Imp function *)
(* Call frame

     | sp                          fp             |
   --+-------------+--------+----+----+-----------+------
     | temp values | locals | ra | fp | arguments |
   --+-------------+--------+----+----+-----------+------
     |               callee frame                 |   caller frame

   The caller is reponsible for pushing/popping the arguments, the callee
   is reponsible for everything else. *)
let tr_function fdef =
  (* Allocation info for local variables and function parameters *)
  (* TODO: replace with an explicit allocation table deduced from [allocate_locals] *)
  let alloc, r_max = allocate_locals fdef in
  (*let alloc = Hashtbl.create 16 in
  
  List.iteri (fun k id -> Hashtbl.add alloc id (4*(k+1))) fdef.params;
  List.iteri (fun k id -> Hashtbl.add alloc id (-4*(k+2))) fdef.locals;*)
  
  (* Generate Mips code for an Imp expression. The generated code produces the
     result in register $ti, and do not alter registers $tj with j < i. *)
  let rec tr_expr i e =
    if i < nb_tmp_regs then
    let ti = 
       tmp_regs.(i) 
    in 
    match e with
    | Cst(n)  -> li ti n
    | Bool(b) -> if b then li ti 1 else li ti 0
    | Var(x) -> 
      (* TODO: replace to take into account explicit allocation info *)
      (match Hashtbl.find_opt alloc x with
       | Some (Reg reg)-> move reg ti 
       | Some (Stack offset) -> lw ti (offset)(fp)
       | None -> la ti x @@ lw ti 0(ti)) (* non-local assumed to be a valid global *)
    | Binop(bop, e1, e2) ->
       let op = match bop with
         | Add -> add
         | Mul -> mul
         | Lt  -> slt
       in
       tr_expr i e1 @@ tr_expr (i+1) e2 @@ op ti ti tmp_regs.(i+1)

    (* Function call.
       Evaluate the arguments and push their values onto the stack.
       Save all temporary registers with number < i
       Jump to the function.
       Finallly restore saved tempary registers and clean the stack. *)
    | Call(f, params) ->
      let len = List.length fdef.params in
      tr_params i params @@ save_tmp (i-1)  
      @@ save_a_reg (min 3 (List.length fdef.params))
      @@ jal f 
      @@ restore_a_reg (min 3 (List.length fdef.params))
      @@ restore_tmp (i-1) @@ addi sp sp (4 * len)
    else
      let offset = -4 * (nb_tmp_regs - 1) in
      save_tmp (i - 1)
      @@ tr_expr 0 e @@ sw t0 offset(fp)
      @@ restore_tmp (i - 1)

  and tr_params i = function
    | []        -> nop
    | e::params when i >= 4 -> 
      tr_params i params @@ tr_expr i e @@ push tmp_regs.(i)
    | e::params -> 
       tr_params (i + 1) params @@ tr_expr i e @@ push a_regs.(i) @@ move a_regs.(i) tmp_regs.(i)


  in

  (* Generate new labels for jumps *)
  let new_label =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "__%s_%i" fdef.name !cpt
  in
  

  (* Generate MIPS code for an Imp instruction or sequence. *)
  let rec tr_seq = function
    | []   -> nop
    | i::s -> tr_instr i @@ tr_seq s

  and tr_instr = function
    | Putchar(e) -> tr_expr 0 (simplify_expr e) @@ move a0 t0 @@ li v0 11 @@ syscall
    | Set(x, e) ->
       (* TODO: replace to take into account explicit allocation info *)
       let set_code = match Hashtbl.find_opt alloc x with
         | Some Reg reg -> move t0 reg
         | Some Stack offset -> sw t0 offset(fp)
         | None -> la t1 x @@ sw t0 0(t1)
       in
       tr_expr 0 (simplify_expr e) @@ set_code

    | If(c, s1, s2) ->
       let then_label = new_label()
       and end_label = new_label()
       in
       tr_expr 0 (simplify_expr c) @@ bnez t0 then_label
       (* fall to else branch *) @@ tr_seq s2 @@ b end_label
       @@ label then_label @@ tr_seq s1 (* fall through *)
       @@ label end_label

    | While(c, s) ->
       let test_label = new_label()
       and code_label = new_label()
       in
       b test_label
       @@ label code_label @@ tr_seq s
       @@ label test_label @@ tr_expr 0 (simplify_expr c) @@ bnez t0 code_label
       (* fall through *)

    (* Return from a call with a value. Includes cleaning the stack. *)
    | Return(e) -> tr_expr 0 (simplify_expr e) @@ addi sp fp (-4) @@ pop ra @@ pop fp @@ jr ra
    | Expr(e) -> tr_expr 0 (simplify_expr e)
  in

  let save_code = save var_regs  (nb_var_regs-1) in
  let restore_code =  restore var_regs (nb_var_regs-1)   in

  (* Mips code for the function itself. 
     Initialize the stack frame and save callee-saved registers, run the code of 
     the function, then restore callee-saved, clean the stack and returns with a 
     dummy value if no explicit return met. *)
  push fp @@ push ra @@ addi fp sp 4
  (* TODO: replace the following, to save callee-saved registers and allocate 
     the right number of slots on the stack for spilled local variables *)
  @@ save_code
  @@ addi sp sp (-4 * List.length fdef.locals)
  @@ tr_seq fdef.code
  (* TODO: restore callee-saved registers *)
  @@ restore_code
  @@ addi sp fp (-4) 
  @@ pop ra @@ pop fp @@ li t0 0 @@ jr ra

(* Generate Mips code for an Imp program. *)
let translate_program prog =
  let init = 
    beqz a0 "init_end" @@ lw a0 0(a1) @@ jal "atoi"
    @@ label "init_end" @@ push v0
    @@ jal "main"
    @@ li v0 10 @@ syscall
  and built_ins =
    comment "built-in atoi"
    @@ label "atoi" @@ li v0 0
    @@ label "atoi_loop" @@ lbu t0 0(a0) @@ beqz t0 "atoi_end"
    @@ addi t0 t0 (-48) @@ bltz t0 "atoi_error" @@ bgei t0 10 "atoi_error"
    @@ muli v0 v0 10 @@ add v0 v0 t0 @@ addi a0 a0 1 @@ b "atoi_loop"
    @@ label "atoi_error" @@ li v0 10 @@ syscall
    @@ label "atoi_end" @@ jr ra
  in

  let function_codes = List.fold_left
    (fun code fdef -> code @@ label fdef.name @@ tr_function fdef)
    nop prog.functions
  in
  let text = init @@ function_codes @@ built_ins in
  let data = List.fold_left
      (fun code id -> code @@ label id @@ dword [0])
      nop prog.globals
  in
  
  { text; data }
