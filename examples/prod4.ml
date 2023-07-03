(* Product of Fig. 4.5 p. 54 *)

open Unfoldings.Product
open Unfoldings.Product.Trans

let s = of_lists
  ["s1";"s2"]
  [[T "t1"]]
  [(["s1"] --> ["s2"]) [T "t1"]]
  ["s1"]

let r = of_lists
  ["r1";"r2";"r3"]
  [[T "u1"];[T "u2"]]
  [
    (["r1"] --> ["r2"]) [T "u1"];
    (["r2"] --> ["r3"]) [T "u2"];
  ]
  ["r1"]

let q = of_lists
  ["q1";"q2"]
  [[T "v1"]]
  [(["q1"] --> ["q2"]) [T "v1"]]
  ["q1"]

let prod4 = product [s;r;q]
  [
    [T "t1"; T "u1"; Idle];
    [Idle; T "u1"; T "v1"];
    [T "t1"; T "u2"; Idle];
    [Idle; T "u2"; T "v1"];
  ]
