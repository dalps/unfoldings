(* Transition system of Fig. 4.1 p. 44 *)

open Unfoldings.Lts

let lts2 = of_lists
  ["s1";"s2";"s3";"s4"]
  ["t1";"t2";"t3";"t4";"t5"]
  [
    (["s1"] --> ["s2"]) "t1";
    (["s1"] --> ["s3"]) "t2";
    (["s2"] --> ["s3"]) "t3";
    (["s3"] --> ["s2"]) "t4";
    (["s3"] --> ["s4"]) "t5";
  ]
  ["s1"]