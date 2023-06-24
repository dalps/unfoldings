(* Product of Fig. 2.3 p. 9 *)

open Unfoldings.Product
open Unfoldings.Product_transition

let prod1 = of_lists
  ["p1";"p2";"p3";"p4"]
  [[T "e1"];[T "e2"];[T "e3"]]
  [
    "p1" @--> [T "e2"];
    "p2" @--> [T "e2"];
    "p3" @--> [T "e1"];
    "p4" @--> [T "e3"];
    [T "e1"] -->@ "p1";
    [T "e2"] -->@ "p3";
    [T "e2"] -->@ "p4";
    [T "e3"] -->@ "p2";
  ]
  ["p1";"p2"]
