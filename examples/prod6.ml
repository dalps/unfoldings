(* Product of Fig. 6.1 p. 99 *)

open Unfoldings.Lts

let prod6 = product_component_of (build
  ["s1";"s2";"s3";"s4";"s5";"s6"]
  [
    ("s1" --> "s2") "a";
    ("s1" --> "s3") "b";
    ("s3" --> "s2") "c";
    ("s2" --> "s3") "r";
  ]
  "s1")