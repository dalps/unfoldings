open Unfoldings.Petrinet.PetriNet;;

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

assert (is_occurrence_sequence ["e1";"t1";"u1";"e2"] n2 = true);;
assert (is_occurrence_sequence ["e1";"u1";"t1";"e2"] n2 = true);;
assert (is_occurrence_sequence ["e1";"u1";"e2"] n2 = false);;
assert (is_occurrence_sequence ["e2";"t1";"u1";"e1"] n2 = false);;

assert (is_predecessor (E "e1") (E "e2") n2 = true);;
assert (is_predecessor (E "t1") (E "u1") n2 = false);;
assert (is_predecessor (P "s2") (E "u1") n2 = false);;
 
assert (conflicts (P "s2") (P "r2") n2 = false);;
assert (conflicts (E "t1") (E "u1") n2 = false);;

assert (is_concurrent (E "t1") (E "u1") n2 = true);;
assert (is_concurrent (E "e1") (E "u1") n2 = false);;
