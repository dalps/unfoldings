(* Transition system of Fig. 4.1 p. 44 *)

open Unfoldings.Lts

let lts2 = build
  ["s1";"s2";"s3";"s4"]
  [
    ("s1" --> "s2") "t1";
    ("s1" --> "s3") "t2";
    ("s2" --> "s3") "t3";
    ("s3" --> "s2") "t4";
    ("s3" --> "s4") "t5";
  ]
  "s1"