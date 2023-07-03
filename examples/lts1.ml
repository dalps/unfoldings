(* Transition system of Fig. 2.1 p. 6 *)

open Unfoldings.Lts

let lts1 = of_lists
  ["s1";"s2";"s3";"s4"]
  ["t1";"t2";"t3";"t4";"t5"]
  [
    (["s1"] --> ["s2"]) "t1";
    (["s1"] --> ["s3"]) "t2";
    (["s2"] --> ["s4"]) "t3";
    (["s3"] --> ["s4"]) "t4";
    (["s4"] --> ["s1"]) "t5";
  ]
  ["s1"]