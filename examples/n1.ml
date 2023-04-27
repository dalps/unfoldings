(* Example net from EH fig. 2.3 p. 9
   Not a prefix of an unfolding: contains two cycles.
*)

open Unfoldings.Petrinet

let n1 = build
  ["p1";"p2";"p3";"p4"]
  ["e1";"e2";"e3"]
  [
    Flow.to_event "p1" "e2";
    Flow.to_event "p2" "e2";
    Flow.to_event "p3" "e1";
    Flow.to_event "p4" "e3";
    Flow.to_place "e1" "p1";
    Flow.to_place "e2" "p3";
    Flow.to_place "e2" "p4";
    Flow.to_place "e3" "p2";
  ]
  ["p1";"p2"]
