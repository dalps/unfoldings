open Unfoldings.Product_pretrinet;;

module PlaceSet = Set.Make(Unfoldings.State)
module EventSet = Set.Make(Unfoldings.Product_transition)
module FlowSet = Set.Make(Unfoldings.Product_pretrinet.PNet.Flow)
module NodeSet = Set.Make(Unfoldings.Product_pretrinet.PNet.Node)


open Examples.N1;;

fire "e2" n1;; (* {p1,p2} ---> m1 := {p3,p4} *)
fire "e1" n1;; (* {p3,p4} ---> m2 := {p1,p4} *)
fire "e3" n1;; (* {p1,p4} ---> {p1,p2} = m0 *)
fire "e1" n1;; (* not enabled *)
fire "e9" n1;; (* not an event in n *)

assert (is_occurrence_sequence ["e2";"e1";"e3"] n1 = true);;
assert (is_occurrence_sequence ["e2";"e3";"e1"] n1 = true);;
assert (is_occurrence_sequence ["e1"] n1 = false);;
assert (is_occurrence_sequence ["e2";"e3";"e2"] n1 = false);;
assert (is_occurrence_sequence ["e2";"e2"] n1 = false);;



open Examples.N2;;
(*
open Unfoldings.Branching_process
open Unfoldings.Branching_process.BPNet.Node;;
open Unfoldings.Branching_process.BPNet;; 
*)

assert (is_occurrence_sequence ["e1";"t1";"u1";"e2"] n2 = true);;
assert (is_occurrence_sequence ["e1";"u1";"t1";"e2"] n2 = true);;
assert (is_occurrence_sequence ["e1";"u1";"e2"] n2 = false);;
assert (is_occurrence_sequence ["e2";"t1";"u1";"e1"] n2 = false);;

(* #TODO Convert n2 to a branching process to make these tests work *)
(*
assert (is_predecessor (of_trans "e1") (of_trans "e2") n2 = true);;
assert (is_predecessor (of_trans "t1") (of_trans "u1") n2 = false);;
assert (is_predecessor (of_place "s2") (of_trans "u1") n2 = false);;
 
assert (is_conflict (of_place "s2") (of_place "r2") n2 = false);;
assert (is_conflict (of_trans "t1") (of_trans "u1") n2 = false);;

assert (is_concurrent (of_trans "t1") (of_trans "u1") n2 = true);;
assert (is_concurrent (of_trans "e1") (of_trans "u1") n2 = false);;

assert (is_reachable (PlaceSet.of_list ["s1";"r1"]) n2 = true);;
assert (is_reachable (PlaceSet.of_list ["s2";"r2"]) n2 = true);;
assert (is_reachable (PlaceSet.of_list ["s3";"r3"]) n2 = true);;
assert (is_reachable (PlaceSet.of_list ["s4";"r4"]) n2 = true);;
assert (is_reachable (PlaceSet.of_list ["s4";"r4";"s3"]) n2 = false);;
assert (is_reachable (PlaceSet.of_list ["s3";"r2"]) n2 = true);;
assert (is_reachable (PlaceSet.of_list ["r2";"s3"]) n2 = true);;
assert (is_reachable (PlaceSet.of_list ["r1";"r2"]) n2 = false);;
assert (is_reachable (PlaceSet.of_list ["r1"]) n2 = true);; (* questionable *)
assert (is_reachable (PlaceSet.of_list ["s1";"s4"]) n2 = false);;

add_trans "t2" n2;;
add_trans "t3" n2;;
add_place "s2_4" n2;;

add_to_trans_arc "s2" "t2" n2;;
add_to_place_arc "t2" "s2_4" n2;;
add_to_trans_arc "s2_4" "t3" n2;;
add_to_place_arc "t3" "s4" n2;;

assert (is_conflict (of_place "s2_4") (of_place "r3") n2 = false);;
assert (is_conflict (of_place "s2_4") (of_place "s4") n2 = false);;
assert (is_conflict (of_place "s2_4") (of_place "s3") n2 = true);;
assert (is_conflict (of_trans "t1") (of_trans "t2") n2 = true);;
assert (is_conflict (of_trans "e2") (of_place "s2_4") n2 = true);;
assert (is_conflict (of_trans "t3") (of_trans "e2") n2 = true);;
assert (is_conflict (of_trans "t2") (of_trans "e2") n2 = true);;

assert (is_concurrent (of_trans "t3") (of_trans "u1") n2 = true);;
assert (is_concurrent (of_trans "t2") (of_trans "u1") n2 = true);;
assert (is_concurrent (of_trans "t3") (of_trans "e2") n2 = false);;
assert (is_concurrent (of_trans "t2") (of_trans "e2") n2 = false);;
assert (is_concurrent (of_trans "t3") (of_trans "t1") n2 = false);; 
*)



open Examples.S1;;
open Unfoldings.Lts;;

assert (is_computation ["t1";"t2"] s1 = false);;
assert (is_computation ["t1";"t3";"t4"] s1 = false);;
assert (is_computation ["t3";"t5";"t1"] s1 = true);;
assert (is_history ["t3";"t5";"t1"] s1 = false);;
assert (is_history ["t1";"t3";"t5";"t2";"t4";"t5"] s1 = true);;
assert (is_history ["t1";"t3";"t5";"t2";"t4";"t1"] s1 = false);;



open Examples.Prod;;
open Unfoldings.Product_transition;;

assert (is_occurrence_sequence ["t1,_";"_,u1";"t3,u2";"t5,_"] n);;
assert (is_occurrence_sequence ["_,u1";"t1,_";"t3,u2";"_,u3"] n);;
assert (is_occurrence_sequence ["_,u1";"t3,u2"] n = false);;
assert (is_occurrence_sequence ["t1,_";"_,u1";"t4,u2";"_,u3"] n = false);;
assert (is_occurrence_sequence ["t1,_";"_,u1";"t4,u2";"_,u3"] n = false);;
assert (is_occurrence_sequence ["t2,_";"_,u1";"t4,u2";"_,u3"] n);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"_,u3";"t5,_"] n);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"t5,_";"_,u3";"_,u1"] n);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"t5,_";"_,u3";"_,u1";"t4,u2"] n = false);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"t5,_";"_,u3";"t2,_";"_,u1";"t4,u2"] n);;
assert (is_occurrence_sequence ["_,u1";"t2,_";"t4,u2";"t5,_";"_,u3";"t1,_";"_,u1";"t3,u2"] n);;

assert (is_independent "t1,_" "_,u1");;
assert (is_independent "t1,_" "t5,_" = false);;
assert (is_independent "t3,u2" "t4,u2" = false);;

assert (is_equivalent ["t1,_";"_,u1";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"_,u3";"t5,_"]);;
assert (is_equivalent ["t1,_";"_,u1";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"]);;
assert (is_equivalent ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"_,u3";"t5,_"]);;
assert (is_equivalent ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"]);;
assert (is_equivalent ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t4,_";"_,u3"] = false);;
assert (is_equivalent ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t4,_"] = false);;
assert (is_equivalent ["_,u1";"t1,_";"t3,u2";"t5,_";"_,u3"] ["_,u1";"t1,_";"t3,u2";"t5,_";"t4,u2"] = false);;
