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
      live.(i.nb) <- set; set
    | Set(x, e) -> 
      let use_e = use_expr e in
      let set =  VSet.union use_e (VSet.diff lv_out (VSet.singleton x)) in
      live.(i.nb) <- set; set
    | Return e ->  let use_e = use_expr e in
        let set = use_e in
        live.(i.nb) <- set; set
    | While(b, s) -> let use_b = use_expr b in
        let seq = lv_in_list s lv_out in
        let set = VSet.union use_b seq in
        live.(i.nb) <- set; set
    | If(b, s1, s2) -> let use_b = use_expr b in
      let seq1 = lv_in_list s1 lv_out in
      let seq2 = lv_in_list s2 lv_out in
      let set = VSet.union use_b (VSet.union seq1 seq2) in
      live.(i.nb) <- set; set
  (* the same for a sequence, and records in [live] the live sets computed
     on entry to each analyzed instruction *)
  and lv_in_list l lv_out = 
    List.fold_left (fun acc i -> VSet.union acc (lv_in_instr i lv_out)) VSet.empty l
  in
  let _ = lv_in_list fdef.code VSet.empty in
  live

let liveness_intervals_from_liveness fdef =
  let live = liveness fdef in
  (* for each variable [x], create the smallest interval that contains all
     the numbers of instructions where [x] is live *)
  let intervals = ref [] in

  let add_intervals var min_instruction max_instruction =
    intervals := (var, min_instruction,max_instruction)::!intervals
  in

  Array.iteri (fun instr_num live_set ->
    VSet.iter (fun var ->
      let rec find_interval start_instr end_instr curr_instr =
        if curr_instr > Array.length live - 1 || not (VSet.mem var live.(curr_instr)) then
          add_intervals var start_instr (curr_instr - 1)
        else
          find_interval start_instr curr_instr (curr_instr + 1)
      in
      find_interval instr_num instr_num instr_num
    ) live_set
  ) live;

  !intervals



