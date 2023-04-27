(* Example net from EH fig. 2.3 p. 9
   Not a prefix of an unfolding: contains two cycles.
*)

open Unfoldings.Petrinet

let n1 = PetriNet.build
  ["p1";"p2";"p3";"p4"]
  ["e1";"e2";"e3"]
  [
    {source = (P "p1"); target = (E "e2")};
    {source = (P "p2"); target = (E "e2")};
    {source = (P "p3"); target = (E "e1")};
    {source = (P "p4"); target = (E "e3")};
    {source = (E "e1"); target = (P "p1")};
    {source = (E "e2"); target = (P "p3")};
    {source = (E "e2"); target = (P "p4")};
    {source = (E "e3"); target = (P "p2")}
  ]
  ["p1";"p2"]
