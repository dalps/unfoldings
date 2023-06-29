open Examples.Prod1;;
open Unfoldings.Product;;

fire [T "e2"] prod1;; (* {p1,p2} ---> m1 := {p3,p4} *)
fire [T "e1"] prod1;; (* {p3,p4} ---> m2 := {p1,p4} *)
fire [T "e3"] prod1;; (* {p1,p4} ---> {p1,p2} = m0 *)
fire [T "e1"] prod1;; (* not enabled *)
fire [T "e9"] prod1;; (* not an event in n *)

assert (is_occurrence_sequence [[T "e2"];[T "e1"];[T "e3"]] prod1 = true);;
assert (is_occurrence_sequence [[T "e2"];[T "e3"];[T "e1"]] prod1 = true);;
assert (is_occurrence_sequence [[T "e1"]] prod1 = false);;
assert (is_occurrence_sequence [[T "e2"];[T "e3"];[T "e2"]] prod1 = false);;
assert (is_occurrence_sequence [[T "e2"];[T "e2"]] prod1 = false);;

print_endline "[OK] prod1";;

(* --- *)

open Examples.Bp1;;
open Unfoldings.Branching_process;;
open Unfoldings.Branching_process.Node;;

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

let t2 = Event.build 5 [] [T "t2"];;
let t3 = Event.build 6 [] [T "t3"];;
let s2_4 = LabelledPlace.build [] "s2_4";;

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
open Unfoldings.Product;;
open Trans;;
open Unfoldings.History_utils

let t1 = [T "t1"; Idle];;
let t2 = [T "t2"; Idle];;
let u1 = [Idle; T "u1"];;
let t3u2 = [T "t3"; T "u2"];;
let t4u2 = [T "t4"; T "u2"];;
let t4 = [T "t4"; Idle];;
let t5 = [T "t5"; Idle];;
let u3 = [Idle; T "u3"];;

assert (is_occurrence_sequence [t1;u1;t3u2;t5] prod2);;
assert (is_occurrence_sequence [u1;t1;t3u2;u3] prod2);;
assert (is_occurrence_sequence [u1;t3u2] prod2 = false);;
assert (is_occurrence_sequence [t1;u1;t4u2;u3] prod2 = false);;
assert (is_occurrence_sequence [t1;u1;t4u2;u3] prod2 = false);;
assert (is_occurrence_sequence [t2;u1;t4u2;u3] prod2);;
assert (is_occurrence_sequence [u1;t2;t4u2;u3;t5] prod2);;
assert (is_occurrence_sequence [u1;t2;t4u2;t5;u3;u1] prod2);;
assert (is_occurrence_sequence [u1;t2;t4u2;t5;u3;u1;t4u2] prod2 = false);;
assert (is_occurrence_sequence [u1;t2;t4u2;t5;u3;t2;u1;t4u2] prod2);;
assert (is_occurrence_sequence [u1;t2;t4u2;t5;u3;t1;u1;t3u2] prod2);;

assert (is_independent t1 u1);;
assert (is_independent t1 t5 = false);;
assert (is_independent t3u2 t4u2 = false);;

assert (tword_equiv [t1;u1;t3u2;t5;u3] [u1;t1;t3u2;u3;t5]);;
assert (tword_equiv [t1;u1;t3u2;t5;u3] [u1;t1;t3u2;t5;u3]);;
assert (tword_equiv [u1;t1;t3u2;t5;u3] [u1;t1;t3u2;u3;t5]);;
assert (tword_equiv [u1;t1;t3u2;t5;u3] [u1;t1;t3u2;t5;u3]);;
assert (tword_equiv [u1;t1;t3u2;t5;u3] [u1;t1;t3u2;t4;u3] = false);;
assert (tword_equiv [u1;t1;t3u2;t5;u3] [u1;t1;t3u2;t4] = false);;
assert (tword_equiv [u1;t1;t3u2;t5;u3] [u1;t1;t3u2;t5;t4u2] = false);;

let a = [T "a"; Idle; Idle];;
let b = [Idle; T "b"; Idle];;
let c = [Idle; Idle; T "c"];;
let c1 = [Idle; Idle; T "c1"];;

assert (tword_equiv [a;b;c] [b;a;c]);;
assert (tword_equiv [a;b;c] [a;c;b]);;
assert (tword_equiv [b;a;c] [a;c;b]);;
assert (tword_equiv [a;b;c] [a;c1;b] = false);;
assert (tword_equiv [a;b;c] [c;b;a] = false);;

print_endline "[OK] prod2";;

(* --- *)

let d = [T "d"; Idle; Idle];;
let ef = [Idle; T "e"; T "f"];;
let gc = [T "g"; Idle; T "c"];;
let t4u4 = [T "t4"; T "u4"];;

let h1 = [t1;u1;t3u2;t5;u3];;
let h21 = [a;b;c];;
let h22 = [d;ef;gc];;
let h3 = [t1;u1;t5;u3;t1;u1;t3u2;t5;u3;t4u4];;
let h4 = [[T "t"]];;

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

let e1 = [u1];;
let e2 = [t1];;
let e6 = [u1;t1;t3u2;u3;u1];;
let e7 = [t2;u1;t4u2];;

let h1 = [u1;t1];;
let h2 = [t1;u1];;

assert(d_compare sl_compare e1 e2 < 0);;
assert(d_compare sl_compare e1 e2 < 0);;
assert(d_compare sl_compare e6 e7 < 0);;
assert(d_compare sl_compare e1 e6 < 0);;
assert(d_compare sl_compare e2 e6 < 0);;
assert(d_compare sl_compare e1 e7 < 0);;
assert(d_compare sl_compare e2 e7 < 0);;
assert(d_compare sl_compare h1 h2 = 0);;

print_endline "[OK] misc";;

(* --- *)

open Unfoldings.Unfold;;
open Examples.Prod1;;
open Examples.Prod2;;
open Examples.Prod3;;
open Examples.Prod4;;
open Examples.Prod5;;

let p = prod2;;

let n0 = unfold_init p;;

let n1s = unfold_1 n0 1 p;;
assert (List.length n1s = 3);;
let n1 = List.hd n1s;;

let n2s = unfold_1 n1.prefix 2 p;;
assert (List.length n2s = 1);;
let n2 = List.nth n2s 0;;

let n3s = unfold_1 n2.prefix 3 p;;
assert (List.length n3s = 1);;
let n3 = List.nth n3s 0;;

let n4s = unfold_1 n3.prefix 4 p;;
assert (List.length n4s = 2);;
let n4 = List.nth n4s 1;;

let n5s = unfold_1 n4.prefix 5 p;;
assert (List.length n5s = 2);;
let n5 = List.nth n5s 0;;

let n6s = unfold_1 n5.prefix 6 p;;
assert (List.length n6s = 3);;

print_endline "[OK] unfold prod2";;

(* --- *)

assert(is_executable prod1 (d_compare sl_compare) [[T "e1"]] 3);;
assert(is_executable prod1 (d_compare sl_compare) [[T "e2"]] 3);;
assert(is_executable prod1 (d_compare sl_compare) [[T "e3"]] 3);;
print_endline "[OK] is_executable prod1";;

assert(is_executable prod2 (d_compare sl_compare) [u3] 5);;
assert(is_executable prod2 (d_compare sl_compare) [t4u2] 10);;
assert(is_executable prod2 (d_compare sl_compare) [t5] 10);;
assert(is_executable prod2 (d_compare sl_compare) [t3u2] 5);;
print_endline "[OK] is_executable prod2";;

let a0a1 = [T "a0"; T "a1"; Idle; Idle; Idle];;
let b1 = [Idle; T "b1"; Idle; Idle; Idle];;
let b2 = [Idle; Idle; T "b2"; Idle; Idle];;
let b3 = [Idle; Idle; Idle; T "b3"; Idle];;
let b4 = [Idle; Idle; Idle; Idle; T "b4"];;
let c = [T "c0"; T "c1"; T "c2"; T "c3"; T "c4"];;

assert(is_executable prod3 (d_compare sl_compare) [b1] 5);;
assert(is_executable prod3 (d_compare sl_compare) [b2] 5);;
assert(is_executable prod3 (d_compare sl_compare) [b3] 5);;
assert(is_executable prod3 (d_compare sl_compare) [b4] 5);;
assert(is_executable prod3 (d_compare sl_compare) [a0a1] 5);;
assert(is_executable prod3 (d_compare sl_compare) [c] 5 = false);;
assert(is_executable prod3 (d_compare sl_compare) [c; a0a1] 5);;
print_endline "[OK] is_executable prod3";;

assert(is_executable prod4 (d_compare sl_compare) [[Idle; T "u1"; T "v1"]] 5);;
assert(is_executable prod4 (d_compare sl_compare) [[Idle; T "u1"; Idle]] 5 = false);;
assert(is_executable prod4 (d_compare sl_compare) [[T "t1"; T "u1"; Idle]] 5);;
print_endline "[OK] is_executable prod4";;

assert(is_executable prod5 (d_compare sl_compare) [[Idle; Idle; T "f3"; T "f4"]] 10);;
assert(is_executable prod5 (d_compare sl_compare) [[Idle; Idle; T "f3"; T "f5"]] 10 = false);;
assert(is_executable prod5 (d_compare sl_compare) [[T "i1"; T "i2"; T "i3"; T "i4"]] 15);;
assert(is_executable prod5 sl_compare [[T "i1"; T "i2"; T "i3"; T "i4"]] 15);;
print_endline "[OK] is_executable prod5";;

open Unfoldings.Product;;

assert(TransSet.cardinal (transitions (undoable prod2)) = 2 * (TransSet.cardinal (transitions prod2)));;
assert(FlowSet.cardinal (flow (undoable prod2)) = 2 * (FlowSet.cardinal (flow prod2)));;
assert(PlaceSet.equal (places (undoable prod2)) (places prod2));;
assert(PlaceSet.equal (marking (undoable prod2)) (marking prod2));;

open Examples.Prod2;; (* "reload" prod2 *)

let undo_prod2 = undoable prod2;;

TransSet.iter
  (fun t ->
    let undo_t = undo_of t in
    assert(TransSet.mem undo_t (transitions undo_prod2))
  )
  (transitions prod2);;

let undo_t1 = [U "t1"; Idle];;
let undo_t2 = [U "t2"; Idle];;

let m0 = marking undo_prod2;;

fire t1 undo_prod2;;
fire undo_t1 undo_prod2;;
assert(PlaceSet.equal m0 (marking undo_prod2));; (* as if the marking never changed *)

fire t2 undo_prod2;;
fire undo_t2 undo_prod2;;
assert(PlaceSet.equal m0 (marking undo_prod2));;

fire t2 undo_prod2;;
fire undo_t1 undo_prod2;;
assert(PlaceSet.equal m0 (marking undo_prod2) = false);;

fire undo_t2 undo_prod2;;
fire t1 undo_prod2;;
fire undo_t2 undo_prod2;;
assert(PlaceSet.equal m0 (marking undo_prod2) = false);;
print_endline "[OK] undoable";;