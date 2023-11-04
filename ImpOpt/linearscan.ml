open Imp
open Nimp

(* sort by ascending lower bound, and sort equals by ascending upper bound *)
let sort_2 l =
  List.stable_sort (fun (_, l1, _) (_, l2, _) -> l1 - l2) l
let sort_3 l =
  List.stable_sort (fun (_, _, h1) (_, _, h2) -> h1 - h2) l
let sort_intervals l =
  sort_2 (sort_3 l)

(* insert interval [i] in active list [l] 
   pre/post-condition: sorted by ascending upper bound *)
let rec insert_active i l =
  match l with
  | [] -> [i]
  | hd::tl -> let (_, _, upp1) = i in
              let (_, _, upp2) = hd in
              if upp1 < upp2 then i::l
              else hd::insert_active i tl

(* raw allocation information for a variable *)
type raw_alloc =
  | RegN  of int  (* index of the register *)
  | Spill of int  (* index of the spill *)

let print_alloc interval =
  let raw reg = match reg with
  | RegN n -> Printf.sprintf " Reg %i" n
  | Spill n -> Printf.sprintf " Spill %i" n
  in
  Hashtbl.iter (
    fun x reg -> Printf.printf "Name %s: %s \n" x (raw reg) 
  ) interval
  

(* allocation of the local variables of a function [fdef] using linear scan
   algorithm, with [nb_regs] registers available for allocation ;
   return a raw allocation for each variable, as well as the maximum index of
   used registers, and the number of used stack slots (spills) *)
let lscan_alloc nb_regs fdef =
  let live_intervals = Liveness.liveness_intervals_from_liveness fdef in
  let alloc = Hashtbl.create (List.length fdef.locals) in
  let active = ref [] in
  let free = ref (List.init nb_regs (fun i -> i)) in
  let r_max = ref (-1) in (* maximum index of used register *)
  let spill_count = ref 0 in (* number of spilled variables *)
  (* free registers allocated to intervals that stop before timestamp a,
     returns remaining intervals *)
  let rec expire a l = match l with
    | [] -> []
    | interval :: rest -> let (vars, reg , upp) = interval in
      if upp < a then
        let () =  match reg with
        | RegN n -> free := List.sort (fun x  y-> x - y) (n::!free)
        | Spill n -> ()
        in
        expire a rest
      else
      interval :: expire a rest
  in
  (* for each interval i, in sorted order *)
  List.iter (fun interval ->
      let xi, li, hi = interval in
      (* free registers that expire before the lower bound of i *)
      let still_active = expire li !active in
      (* if there are available registers *)
      match !free with
        (* ... then allocate one *)
      | reg::rest -> 
        active := insert_active (xi, RegN(reg), hi) !active;
        r_max := max !r_max reg;
        Hashtbl.add alloc xi (RegN(reg));
        free := rest;
        (* otherwise, may replace an already used register if this can
           make this register available again earlier *)
      | [] -> ( match still_active with
          | (xi2, _ , hi2)::_  when hi2 > hi -> 
            let reg = match List.find_opt (fun (_, r, _) -> (
              match r with 
              | Spill _ -> false
              | RegN _ -> true
            )) still_active with
            | Some (_, r, _) -> (match r with RegN(reg) -> reg | _ -> failwith "Invalid register")
            | None -> failwith "No available register"
            in
            Hashtbl.add alloc xi2 (RegN(reg));
            Hashtbl.add alloc xi (Spill(!spill_count));
            active := (xi, RegN(reg), hi) :: List.filter (fun (xi, _, _) -> xi <> xi2) !active;
            active := insert_active (xi2, Spill(!spill_count), hi2) !active;
          | _ ->
          Hashtbl.replace alloc xi (Spill(!spill_count));
          spill_count := !spill_count + 1;
      )
    ) (sort_intervals live_intervals);
  alloc, !r_max, !spill_count
