open Imp
open Nimp

module VSet = Set.Make(String)

(* returns the set of variables accessed by the expression [e] *)
let rec use_expr e =
  match e with
  | Cst(_) -> VSet.empty
  | Bool(_) -> VSet.empty
  | Var(x) -> VSet.singleton x
  | Binop(_, e1, e2) -> VSet.union (use_expr e1) (use_expr e2)
  | Call(_, l) ->  List.fold_left (fun acc e -> VSet.union acc (use_expr e)) VSet.empty l

let liveness fdef =
  let n = max_instr_list fdef.code in
  let live = Array.make (n+1) VSet.empty in
  (* returns the set of variable that live in entry to the numbered 
     instruction [i], assuming a set of live variables [lv_out] on 
     exit of [i] *)
  let rec lv_in_instr i lv_out =
    (* by case on the contents of i.instr *)
    match i.instr with
    | Putchar e | Expr e -> 
      let use_e = use_expr e in
      let set = VSet.union use_e lv_out in
      set
    | Set(x, e) -> 
      let use_e = use_expr e in
      let set =  VSet.union use_e (VSet.diff lv_out (VSet.singleton x)) in
      set
    | Return e ->  let use_e = use_expr e in
        let set = use_e in
        set
    | While(b, s) -> let use_b = use_expr b in
        let seq = lv_in_list s lv_out in
        let seq = lv_in_list s seq in
        let set = VSet.union use_b seq in
        set
    | If(b, s1, s2) -> let use_b = use_expr b in
      let seq1 = lv_in_list s1 lv_out in
      let seq2 = lv_in_list s2 lv_out in
      let set = VSet.union use_b (VSet.union seq1 seq2) in
      set
  (* the same for a sequence, and records in [live] the live sets computed
     on entry to each analyzed instruction *)
  and lv_in_list l lv_out = 
    List.fold_left (fun acc i -> let seq = (lv_in_instr i acc) in live.(i.nb) <- seq; 
      seq
    ) lv_out (List.rev l)
  in
  let _ = lv_in_list fdef.code VSet.empty in
  live


let liveness_intervals_from_liveness fdef =
  
  let live = liveness fdef in
  (* for each variable [x], create the smallest interval that contains all
     the numbers of instructions where [x] is live *)
  let intervals = ref [] in
  let processed_vars = ref VSet.empty in

  let add_intervals var min_instruction max_instruction =
    intervals := (var, min_instruction,max_instruction)::!intervals
  in
  let rec find_interval var start_instr end_instr  =
        if end_instr > Array.length live - 1 || not (VSet.mem var live.(end_instr)) then
          add_intervals var start_instr (end_instr - 1)
        else
          find_interval var start_instr  (end_instr + 1)
  in

  Array.iteri (fun instr_num live_set ->
    VSet.iter (fun var ->
      if not (VSet.mem var !processed_vars) then (
        find_interval var instr_num instr_num ;
        processed_vars := VSet.add var !processed_vars
      )
    ) live_set
  ) live;
  (*List.iter (fun (x, lw, upp) -> 
    Printf.printf "Var %s with lower bound %i and upper bound %i\n" x lw upp
    ) !intervals;*)
  !intervals



