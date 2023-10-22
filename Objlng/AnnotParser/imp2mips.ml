open Imp
open Mips



(* To save the (4-byte) value of a register [reg], decrement $sp by 4 bytes,
   then write the value at the new address pointed by $sp.
   Note: operator @@ concatenates MIPS fragments (defined in Mips module). *)
let push reg =
  subi sp sp 4
  @@ sw reg 0 sp
(* Conversely, to retrieve and remove an element from the top of the stack,
   read the value at the address given by $sp, and increment $sp by 4 bytes. *)
let pop  reg =
  lw reg 0 sp
  @@ addi sp sp 4
(* In both cases, the update of $sp guarantees that the next operation on the
   stack will take into account the fact that the stack grew or shrank. *)

(**
   Function producing MIPS code for an IMP function. Function producing MIPS
   code for expressions and instructions will be defined inside.
 *)

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
| DCall(e, l) -> DCall(simplify_expr e, List.map simplify_expr l)
| Alloc e -> Alloc(simplify_expr e)
| Deref e -> Deref(simplify_expr e)
| Addr s -> Addr s


let tr_function fdef =
  
  let env = Hashtbl.create 16 in
  List.iteri (fun k id -> Hashtbl.add env id (4*(k+1))) fdef.params;
  List.iteri (fun k id -> Hashtbl.add env id (-4*(k+2))) fdef.locals;

  
   let regis i = List.nth [t0; t1; t2; t3; t4; t5; t6; t7;] i in
  
  (** 
     Function that generate MIPS code for an IMP expression.
     The function takes a parameter an expression [e] and produces a
     sequence of MIPS instructions that evaluates [e] and put the
     obtained value in register $t0.
   *)
  let rec tr_expr i = function
    (* Case of a constant: load the value in the target register $t0.
       Boolean [true] is coded by 1 and boolean [false] by 0. *)
    | Cst(n)  -> li (regis i) n
    | Bool(b) -> if b then li (regis i) 1 else li (regis i) 0

    (* Case of a variable. Look up the identifier in the local environement
       [env] to know the offset, then read at the obtained offset from the
       base address $fp. *)
    | Var(id) -> begin
        match Hashtbl.find_opt env id with
        | Some offset -> lw (regis i) offset fp
        (* In case the identifier does not appear in [env], we assume we are
           accessing a global variable. We use the identifier as a label and
           read at the corresponding address. *)
        | None -> la (regis i) id @@ lw (regis i) 0(regis i)
      end
   | Addr(id) -> begin
      match Hashtbl.find_opt env id with
      | Some offset ->
        (* Variable is a local variable. Calculate its memory address
           relative to the base pointer ($fp) and load it into the register. *)
        lw (regis i) offset fp
      | None ->
        (* Variable is a global variable. Load its address directly. *)
        la (regis i) id 
      end
    
    (* Binary operation: use the stack to store intermediate values waiting
       to be used. *)
    | Binop(bop, e1, e2) ->
       let op = match bop with
         | Add -> add
         | Mul -> mul
         | Lt  -> slt
       in
       (* Evaluate [e2] *)
       tr_expr i e2
       (* Store the value of [e2] on the stack (it has been put in $t0). *)
       @@ push (regis i)
       (* Evaluer [e1] *)
       @@ tr_expr i e1
       (* Retrieve the value of [e2] that has been waiting on the stack,
          put it in a register other than $t0 (that one contains the value
          of [e1]). *)
       @@ pop (regis (i+1))
       (* Apply the binary operation to $t0 (the value of [e1]) and $t1 (the
          value of [e2]), and put the result in $t0. *)
       @@ op (regis i) (regis i) (regis (i+1))      

    (* Function call.
       Before jumping to the function itself, evaluate all parameters and put
       their values on the stack, from last to first. *)
    | Call(f, params) ->
      let save_registers =
         List.fold_left
           (fun code i -> push (regis i) @@ code)
           nop
           (List.init ((List.length params) - 1) (fun x -> x))
       in
       let restore_registers =
         List.fold_left
           (fun code i -> pop (regis i) @@ code)
           nop
           (List.init ((List.length params) - 1) (fun x -> x))
       in
       (* Evaluate the arguments and pass them on the stack. *)
       let params_code =
         List.fold_right
           (fun e code ->code @@ tr_expr i e @@ push (regis i))
           params nop
       in
       save_registers
       @@ params_code (* STEP 1 *)
       (* optionnally: save caller-saved registers *)
       @@ jal f
       @@ restore_registers
       @@ addi sp sp (4 * List.length params) 
    | DCall(e, params) ->
       (* J'enleve les caller/restore car pour une raison inconnu cela marche correctement*)
       (* Evaluate the arguments and pass them on the stack. *)
       let params_code =
         List.fold_right
           (fun e code ->code @@ tr_expr i e @@ push (regis i))
           params nop
       in
       tr_expr (i+1) e
       @@ params_code
       @@ jalr (regis (i+1))
       @@ addi sp sp (4 * List.length params) 
    | Deref e ->
       tr_expr i e  (* pointer in t0 *)
       @@ lw (regis i) 0(regis i)

    | Alloc e -> (* request e bytes above the heap *)
       tr_expr i e
       @@ move a0 (regis i)
       @@ li v0 9
       @@ syscall   (* sbrk -> shifts the limit of the heap *)
       @@ move (regis i) v0  (* v0 contains the first address of the allocated space *)

  in

  let tr_expr e = tr_expr 0 e in 
  (**
     Auxiliary function for producing unique labels, for use in the
     translation of control structures (if and while).
   *)
  let new_label =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "__%s_%i" fdef.name !cpt
  in

  (**
     Functions that generate MIPS code for an instruction or a sequence.
   *)
  let rec tr_seq = function
    | []   -> nop
    | [i]  -> tr_instr i
    (* If an IMP sequence contains several instructions, concatenate the
       MIPS sequences for each in order. *)
    | i::s -> tr_instr i @@ tr_seq s

  and tr_instr = function
    (* Prints a char. *)
    | Putchar(e) ->
       (* Evaluate expression [e] *)
       tr_expr (simplify_expr e)
       (* Move the value of [e] from $t0 (where it has been produced)
          to $a0 (where syscall expects it). *)
       @@ move a0 t0
       (* Syscall number 11: printing an ASCII character. *)
       @@ li v0 11
       @@ syscall
        
    (* Assignment.
       After evaluation of [e], its value is in $t0.
       Chose the right instruction to update memory depending on the
       local or global nature of the variable [id]. *)
    | Set(id, e) ->
       let set_code = match Hashtbl.find_opt env id with
         | Some offset -> sw t0 offset fp
         | None -> la t1 id @@ sw t0 0 t1
       in
       tr_expr (simplify_expr e) @@ set_code

    (* Conditional *)
    | If(c, s1, s2) ->
       (* Create two labels that will serve as targets for jumps. *)
       let then_label = new_label()
       and end_label = new_label()
       in
       (* Evaluate the condition [c] *)
       tr_expr (simplify_expr c)
       (* If we got a non-zero value, which is interpreted as [true], jump
          to the code fragment of the "then" branch... *)
       @@ bnez t0 then_label
       (* ... otherwise just fall to the next instruction.
          Hence, we put the code of the "else" branch just here. *)
       @@ tr_seq s2
       (* At the end of the "else" branch, jump to the instruction that follows
          the conditional. *)
       @@ b end_label
       (* Code for the "then" branch. *)
       @@ label then_label
       @@ tr_seq s1
       (* At the end of the "then" branch, there is no need to jump, since we
          are precisely at the end of the conditional. Just put here the
          final label, without any explicitly associated instruction (it will
          be associated to the instruction that immadiately follows the 
          conditional). *)
       @@ label end_label

    (* Loop *)
    | While(c, s) ->
       (* Create two labels for jumps. *)
       let test_label = new_label()
       and code_label = new_label()
       in
       (* First instruction: jump to the code that evaluates the condition. *)
       b test_label
       (* Code for the loop body, introduced by its label. *)
       @@ label code_label
       @@ tr_seq s
       (* At the end of a pass through the loop, just fall to the evaluation of
          the condition, that determines whether the loop is executed again. *)
       @@ label test_label
       @@ tr_expr (simplify_expr c)
       (* If the condition is non-zero, jumps back to the beginning of the loop
          body. *)
       @@ bnez t0 code_label
       (* Otherwise, fall to the next instruction, which in this case is the
          instruction that immediately follows the loop. *)
       (* Note: without the instruction [b test_label] at the beginning, we get
          the behaviour of a do-while loop instead of while. *)

    (* Function termination. *)
    | Return(e) ->
       (* Evaluate the value to be returned, in $t0 *)
       tr_expr (simplify_expr e)
       (* Deallocate the part of the stack used for local variables *)
       @@ addi sp fp (-4)
       (* Retrieve the return address *)
       @@ pop ra
       (* Restore the base pointer of the caller *)
       @@ pop fp
       (* Jumps back to the caller *)
       @@ jr ra

    (* Expression used as an instruction.
       Note that the produced MIPS code writes a value in $t0, but
       this value will not be used. *)
    | Expr(e) ->
       tr_expr (simplify_expr e)

    | Write(e1, e2) ->
       tr_expr (simplify_expr e1)  (* t0: pointer *)
       @@ push t0
       @@ tr_expr (simplify_expr e2)  (* t0: value to be written *)
       @@ pop t1
       @@ sw t0 0(t1)
   | Seq (l) -> tr_seq l

            
  in

  (**
     MIPS code produced for the function.
     Reminder: when this code is executed, the parameters of the call have
     already been evaluated and pushed on the stack. Half of the activation
     frame is already built, and we now build the second hald.
   *)
  (* Save the base address of the activation frame of the caller and the
     return address. *)
  push fp  (* STEP 2 *)
  @@ push ra
    (* optionnally: save callee-saved registers *)
  (* Definition of the base pointer of the new activation frame. *)
  @@ addi fp sp 4
  (* Allocate space on the stack for local variables, by just shifting the
     pointer that indicates where the top is. *)
  @@ addi sp sp (-4 * List.length fdef.locals)   (* END STEP 2 *)
  (* After this preamble, we can execute the actual code of the function. *)
  @@ tr_seq fdef.code
  (* If execution reaches the end of this sequence, it means that no [return]
     instruction has been met. We add a MIPS fragment equivalent to [return 0;] *)
  @@ li t0 0      (* STEP 3 *)
  @@ addi sp fp (-4)
    (* optionnally: restore callee-saved registers *)
  @@ pop ra
  @@ pop fp
  @@ jr ra     (* END STEP 3 *)

(**
   Main function for translating a program.
 *)
let translate_program prog =
  (* MIPS fragment that will be placed at the beginning of the produced 
     assembly code, for retrieving one optional integer command-line 
     argument, then jumping to the "main" function. *)
  let init =
    (* At the beginning, $a0 contains the number of command-line arguments.
       If this number is zero, skip the two following lines. *)
    beqz a0 "init_end"
    (* Otherwise, $a1 contains the address of an array of character strings
       containing the command-line arguments. Here we assume only one argument,
       copy the address of the corresponding string in $a0 and call an
       auxiliary function "atoi" (defined below) for converting the string
       into an integer. *)
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ label "init_end"
    (* At the end, the obtained integer is in $v0. Push it on the stack to
       pass it as an argument to "main". *)
    @@ push v0
    @@ jal "main"
    (* After execution of the "main" function, system call for halting the
       program. *)
    @@ li v0 10
    @@ syscall
  and built_ins =
    (* Conversion function string -> int, that iterates on the characters of
       the string. *)
    comment "built-in atoi"
    @@ label "atoi"
    @@ li   v0 0
    @@ label "atoi_loop"
    @@ lbu  t0 0 a0
    @@ beqz t0 "atoi_end"
    @@ addi t0 t0 (-48)
    @@ bltz t0 "atoi_error"
    @@ bgei t0 10 "atoi_error"
    @@ muli v0 v0 10
    @@ add  v0 v0 t0
    @@ addi a0 a0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ jr   ra
  in

  (**
     Main code for producing the MIPS assembly program corresponding to the
     source IMP program.
   *)
  let function_codes = List.fold_right
    (fun fdef code ->
      label fdef.name @@ tr_function fdef @@ code)
    prog.functions nop
  in
  (* Combine the initialization code seen above, we the code produced for each
     function of the source program. *)
  let text = init @@ function_codes @@ built_ins
  (* In the "data" part, introduce a label for each global variable. Here, all
     the variables are initialized at 0. *)
  and data = List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals nop
  in
  
  { text; data }
