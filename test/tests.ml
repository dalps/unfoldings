open Unfoldings.Petrinet;;

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

assert (is_predecessor (Node.of_event "e1") (Node.of_event "e2") n2 = true);;
assert (is_predecessor (Node.of_event "t1") (Node.of_event "u1") n2 = false);;
assert (is_predecessor (Node.of_place"s2") (Node.of_event "u1") n2 = false);;
 
assert (conflicts (Node.of_place"s2") (Node.of_place"r2") n2 = false);;
assert (conflicts (Node.of_event "t1") (Node.of_event "u1") n2 = false);;

assert (is_concurrent (Node.of_event "t1") (Node.of_event "u1") n2 = true);;
assert (is_concurrent (Node.of_event "e1") (Node.of_event "u1") n2 = false);;
