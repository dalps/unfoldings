(* Product of Fig. 4.5 p. 54 *)

open Unfoldings.Lts
open Unfoldings.Product_pretrinet

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

let prod4 = product [s;r;q]
  [
    [T "t1"; T "u1"; Idle];
    [Idle; T "u1"; T "v1"];
    [T "t1"; T "u2"; Idle];
    [Idle; T "u2"; T "v1"];
  ]
