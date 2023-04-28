open Unfoldings.Petrinet;;

module PlaceSet = Set.Make(Place)
module EventSet = Set.Make(Event)
module FlowSet = Set.Make(Flow)
module NodeSet = Set.Make(Node)


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
open Node;;

assert (is_occurrence_sequence ["e1";"t1";"u1";"e2"] n2 = true);;
assert (is_occurrence_sequence ["e1";"u1";"t1";"e2"] n2 = true);;
assert (is_occurrence_sequence ["e1";"u1";"e2"] n2 = false);;
assert (is_occurrence_sequence ["e2";"t1";"u1";"e1"] n2 = false);;

assert (is_predecessor (of_event "e1") (of_event "e2") n2 = true);;
assert (is_predecessor (of_event "t1") (of_event "u1") n2 = false);;
assert (is_predecessor (of_place "s2") (of_event "u1") n2 = false);;
 
assert (is_conflict (of_place "s2") (of_place "r2") n2 = false);;
assert (is_conflict (of_event "t1") (of_event "u1") n2 = false);;

assert (is_concurrent (of_event "t1") (of_event "u1") n2 = true);;
assert (is_concurrent (of_event "e1") (of_event "u1") n2 = false);;

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

add_event "t2" n2;;
add_event "t3" n2;;
add_place "s2_4" n2;;

add_to_event_arc "s2" "t2" n2;;
add_to_place_arc "t2" "s2_4" n2;;
add_to_event_arc "s2_4" "t3" n2;;
add_to_place_arc "t3" "s4" n2;;

assert (is_conflict (of_place "s2_4") (of_place "r3") n2 = false);;
assert (is_conflict (of_place "s2_4") (of_place "s4") n2 = false);;
assert (is_conflict (of_place "s2_4") (of_place "s3") n2 = true);;
assert (is_conflict (of_event "t1") (of_event "t2") n2 = true);;
assert (is_conflict (of_event "e2") (of_place "s2_4") n2 = true);;
assert (is_conflict (of_event "t3") (of_event "e2") n2 = true);;
assert (is_conflict (of_event "t2") (of_event "e2") n2 = true);;

assert (is_concurrent (of_event "t3") (of_event "u1") n2 = true);;
assert (is_concurrent (of_event "t2") (of_event "u1") n2 = true);;
assert (is_concurrent (of_event "t3") (of_event "e2") n2 = false);;
assert (is_concurrent (of_event "t2") (of_event "e2") n2 = false);;
assert (is_concurrent (of_event "t3") (of_event "t1") n2 = false);;