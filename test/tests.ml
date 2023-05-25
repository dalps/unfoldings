module PlaceSet = Set.Make(Unfoldings.State)
module EventSet = Set.Make(Unfoldings.Product_transition)
module FlowSet = Set.Make(Unfoldings.Product_pretrinet.PNet.Flow)
module NodeSet = Set.Make(Unfoldings.Product_pretrinet.PNet.Node)

(* --- *)

open Examples.Prod1;;
open Unfoldings.Product_pretrinet.PNet;;

fire "e2" prod1;; (* {p1,p2} ---> m1 := {p3,p4} *)
fire "e1" prod1;; (* {p3,p4} ---> m2 := {p1,p4} *)
fire "e3" prod1;; (* {p1,p4} ---> {p1,p2} = m0 *)
fire "e1" prod1;; (* not enabled *)
fire "e9" prod1;; (* not an event in n *)

assert (is_occurrence_sequence ["e2";"e1";"e3"] prod1 = true);;
assert (is_occurrence_sequence ["e2";"e3";"e1"] prod1 = true);;
assert (is_occurrence_sequence ["e1"] prod1 = false);;
assert (is_occurrence_sequence ["e2";"e3";"e2"] prod1 = false);;
assert (is_occurrence_sequence ["e2";"e2"] prod1 = false);;

print_endline "[OK] prod1";;

(* --- *)

open Examples.Bp1;;
open Unfoldings.Branching_process
open Unfoldings.Branching_process.BPNet;; 
open Unfoldings.Branching_process.BPNet.Node;;

assert (is_occurrence_sequence [e1;t1;u1;e2] bp1 = true);;
assert (is_occurrence_sequence [e1;u1;t1;e2] bp1 = true);;
assert (is_occurrence_sequence [e1;u1;e2] bp1 = false);;
assert (is_occurrence_sequence [e2;t1;u1;e1] bp1 = false);;

assert (is_predecessor (of_trans e1) (of_trans e2) bp1 = true);;
assert (is_predecessor (of_trans t1) (of_trans u1) bp1 = false);;
assert (is_predecessor (of_place s2) (of_trans u1) bp1 = false);;
 
assert (is_conflict (of_place s2) (of_place r2) bp1 = false);;
assert (is_conflict (of_trans t1) (of_trans u1) bp1 = false);;

assert (is_concurrent (of_trans t1) (of_trans u1) bp1 = true);;
assert (is_concurrent (of_trans e1) (of_trans u1) bp1 = false);;

assert (is_reachable (PlaceSet.of_list [s1;r1]) bp1 = true);;
assert (is_reachable (PlaceSet.of_list [s2;r2]) bp1 = true);;
assert (is_reachable (PlaceSet.of_list [s3;r3]) bp1 = true);;
assert (is_reachable (PlaceSet.of_list [s4;r4]) bp1 = true);;
assert (is_reachable (PlaceSet.of_list [s4;r4;s3]) bp1 = false);;
assert (is_reachable (PlaceSet.of_list [s3;r2]) bp1 = true);;
assert (is_reachable (PlaceSet.of_list [r2;s3]) bp1 = true);;
assert (is_reachable (PlaceSet.of_list [r1;r2]) bp1 = false);;
assert (is_reachable (PlaceSet.of_list [r1]) bp1 = true);; (* questionable *)
assert (is_reachable (PlaceSet.of_list [s1;s4]) bp1 = false);;

let t2 = Unfoldings.Event.build 5 "t2";;
let t3 = Unfoldings.Event.build 6 "t3";;
let s2_4 = Unfoldings.Labelled_place.build 9 "s2_4";;

add_trans t2 bp1;;
add_trans t3 bp1;;
add_place s2_4 bp1;;

add_to_trans_arc s2 t2 bp1;;
add_to_place_arc t2 s2_4 bp1;;
add_to_trans_arc s2_4 t3 bp1;;
add_to_place_arc t3 s4 bp1;;

assert (is_conflict (of_place s2_4) (of_place r3) bp1 = false);;
assert (is_conflict (of_place s2_4) (of_place s4) bp1 = false);;
assert (is_conflict (of_place s2_4) (of_place s3) bp1 = true);;
assert (is_conflict (of_trans t1) (of_trans t2) bp1 = true);;
assert (is_conflict (of_trans e2) (of_place s2_4) bp1 = true);;
assert (is_conflict (of_trans t3) (of_trans e2) bp1 = true);;
assert (is_conflict (of_trans t2) (of_trans e2) bp1 = true);;

assert (is_concurrent (of_trans t3) (of_trans u1) bp1 = true);;
assert (is_concurrent (of_trans t2) (of_trans u1) bp1 = true);;
assert (is_concurrent (of_trans t3) (of_trans e2) bp1 = false);;
assert (is_concurrent (of_trans t2) (of_trans e2) bp1 = false);;
assert (is_concurrent (of_trans t3) (of_trans t1) bp1 = false);; 

print_endline "[OK] bp1";;

(* --- *)

open Examples.Lts1;;
open Unfoldings.Lts;;

assert (is_computation ["t1";"t2"] lts1 = false);;
assert (is_computation ["t1";"t3";"t4"] lts1 = false);;
assert (is_computation ["t3";"t5";"t1"] lts1 = true);;
assert (is_history ["t3";"t5";"t1"] lts1 = false);;
assert (is_history ["t1";"t3";"t5";"t2";"t4";"t5"] lts1 = true);;
assert (is_history ["t1";"t3";"t5";"t2";"t4";"t1"] lts1 = false);;

print_endline "[OK] lts1";;

(* --- *)

open Examples.Prod2;;
open Unfoldings.Product_transition;;
open Unfoldings.Product_pretrinet.PNet;;

assert (is_occurrence_sequence ["t1,_";"_,u1";"t3,u2";"t5,_"] prod2);;
assert (is_occurrence_sequence ["_,u1";"t1,_";"t3,u2";"_,u3"] prod2);;
assert (is_occurrence_sequence ["_,u1";"t3,u2"] prod2 = false);;
assert (is_occurrence_sequence ["t1,_";"_,u1";"t4,u2";"_,u3"] prod2 = false);;
assert (is_occurrence_sequence ["t1,_";"_,u1";"t4,u2";"_,u3"] prod2 = false);;
assert (is_occurrence_sequence ["t2,_";"_,u1";"t4,u2";"_,u3"] prod2);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"_,u3";"t5,_"] prod2);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"t5,_";"_,u3";"_,u1"] prod2);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"t5,_";"_,u3";"_,u1";"t4,u2"] prod2 = false);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"t5,_";"_,u3";"t2,_";"_,u1";"t4,u2"] prod2);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"t5,_";"_,u3";"t1,_";"_,u1";"t3,u2"] prod2);;

assert (is_independent "t1,_" "_,u1");;
assert (is_independent "t1,_" "t5,_" = false);;
assert (is_independent "t3,u2" "t4,u2" = false);;

assert (tword_equiv ["t1,_";"_,u1";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"_,u3";"t5,_"]);;
assert (tword_equiv ["t1,_";"_,u1";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"]);;
assert (tword_equiv ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"_,u3";"t5,_"]);;
assert (tword_equiv ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"]);;
assert (tword_equiv ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t4,_";"_,u3"] = false);;
assert (tword_equiv ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t4,_"] = false);;
assert (tword_equiv ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t5,_";"t4,u2"] = false);;
assert (tword_equiv ["a,_,_";"_,b,_";"_,_,c"] ["_,b,_";"a,_,_";"_,_,c"]);;
assert (tword_equiv ["a,_,_";"_,b,_";"_,_,c"] ["a,_,_";"_,_,c";"_,b,_"]);;
assert (tword_equiv ["_,b,_";"a,_,_";"_,_,c"] ["a,_,_";"_,_,c";"_,b,_"]);;
assert (tword_equiv ["a,_,_";"_,b,_";"_,_,c"] ["a,_,_";"_,_,c1";"_,b,_"] = false);;
assert (tword_equiv ["a,_,_";"_,b,_";"_,_,c"] ["_,_,c";"_,b,_";"a,_,_"] = false);;

print_endline "[OK] prod2";;

(* --- *)

let h1 = ["t1,_";"_,u1";"t3,u2";"t5,_";"_,u3"];;
let h21 = ["a,_,_";"_,b,_";"_,_,c"];;
let h22 = ["d,_,_";"_,e,f";"g,_,c"];;
let h3 = ["t1,_";"_,u1";"t5,_";"_,u3";"t1,_";"_,u1";"t3,u2";"t5,_";"_,u3";"t4,u4"];;
let h4 = ["t"];;

let traceh3 = trace h3;;
let traceh21 = trace h21;;
let traceh22 = trace h22;;

let test_trace_equiv t = List.fold_left
  (fun b v -> List.fold_left (fun c w -> tword_equiv w v && c) b t)
  true 
  t
;;

let test_trace_proj i t = List.fold_left
  (fun b v -> List.fold_left 
    (fun c w -> projection i w = projection i v && c) b t)
  true 
  t
;;

let naive_concat ws vs =
  List.fold_left
  (fun traces w ->
    (List.fold_left
    (fun traces v -> [w @ v] @ traces)
    []
    vs) @ traces)
  []
  ws;;

assert (test_trace_equiv (trace h1));;
assert (test_trace_equiv (trace h21));;
assert (test_trace_equiv traceh22);;
assert (test_trace_equiv traceh3);;
assert (test_trace_equiv (trace h4));;

assert (test_trace_proj 0 traceh3);;
assert (test_trace_proj 1 traceh3);;

assert (test_trace_proj 0 traceh21);;
assert (test_trace_proj 1 traceh21);;
assert (test_trace_proj 2 traceh21);;

assert (test_trace_proj 0 traceh22);;
assert (test_trace_proj 1 traceh22);;
assert (test_trace_proj 2 traceh22);;

assert (naive_concat (trace h1) (trace h3) <> trace (h1 @ h3));; (* rhs is larger *)
assert (naive_concat (trace h21) (trace h22) <> trace (h21 @ h22));; (* rhs is larger *)

(* --- *)

let e1 = ["_,u1"];;
let e2 = ["t1,_"];;
let e6 = ["_,u1";"t1,_";"t3,u2";"_,u3";"_,u1"];;
let e7 = ["t2,_";"_,u1";"t4,u2"];;

assert(d_compare sl_compare e1 e2 < 0);;
assert(d_compare sl_compare e6 e7 < 0);;
assert(d_compare sl_compare e1 e6 < 0);;
assert(d_compare sl_compare e2 e6 < 0);;
assert(d_compare sl_compare e1 e7 < 0);;
assert(d_compare sl_compare e2 e7 < 0);;

(* --- *)

open Unfoldings.Unfold;;
open Examples.Prod2;;

let p = prod2;;

let n0 = unfold_init p;;

let n1s = unfold_1 n0 1 p sl_compare;;
assert (List.length n1s = 3);;
let n1 = snd (List.hd n1s);;

let n2s = unfold_1 n1 2 p sl_compare;;
assert (List.length n2s = 1);;
let n2 = snd (List.nth n2s 0);;

let n3s = unfold_1 n2 3 p sl_compare;;
assert (List.length n3s = 1);;
let n3 = snd (List.nth n3s 0);;

let n4s = unfold_1 n3 4 p sl_compare;;
assert (List.length n4s = 2);;
let n4 = snd (List.nth n4s 1);;

let n5s = unfold_1 n4 5 p sl_compare;;
assert (List.length n5s = 2);;
let n5 = snd (List.nth n5s 0);;

let n6s = unfold_1 n5 6 p sl_compare;;
assert (List.length n6s = 3);;

print_endline "[OK] unfold prod2";;
