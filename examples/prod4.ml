(* Product of Fig. 4.5 p. 54 *)

open Unfoldings.Lts

let s = product_component_of (build
  ["s1";"s2"]
  [("s1" --> "s2") "t1"]
  "s1")

let r = product_component_of (build
  ["r1";"r2";"r3"]
  [
    ("r1" --> "r2") "u1";
    ("r2" --> "r3") "u2";
  ]
  "r1")

let q = product_component_of (build
  ["q1";"q2"]
  [("q1" --> "q2") "v1"]
  "q1")


open Unfoldings.Product_pretrinet

let prod4 = product [s;r;q]
  [
    [Some "t1"; Some "u1"; None];
    [None; Some "u1"; Some "v1"];
    [Some "t1"; Some "u2"; None];
    [None; Some "u2"; Some "v1"];
  ]
