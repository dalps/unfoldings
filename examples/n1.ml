(* Example net from EH fig. 2.3 p. 9
   Not a prefix of an unfolding: contains two cycles.
*)

open Unfoldings.Product_pretrinet.PNet
open Flow

let n1 = of_lists
  ["p1";"p2";"p3";"p4"]
  ["e1";"e2";"e3"]
  [
    "p1" @--> "e2";
    "p2" @--> "e2";
    "p3" @--> "e1";
    "p4" @--> "e3";
    "e1" -->@ "p1";
    "e2" -->@ "p3";
    "e2" -->@ "p4";
    "e3" -->@ "p2";
  ]
  ["p1";"p2"]
