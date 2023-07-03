(* Product of Fig. 2.2 p. 7 *)

open Unfoldings.Product
open Unfoldings.Product.Trans

let s = of_lists
  ["s1";"s2";"s3";"s4"]
  [[T "t1"];[T "t2"];[T "t3"];[T "t4"];[T "t5"]]
  [
    (["s1"] --> ["s2"]) [T "t1"];
    (["s2"] --> ["s4"]) [T "t3"];
    (["s1"] --> ["s3"]) [T "t2"];
    (["s3"] --> ["s4"]) [T "t4"];
    (["s4"] --> ["s1"]) [T "t5"];
  ]
  ["s1"]

let r = of_lists
  ["r1";"r2";"r3"]
  [[T "u1"];[T "u2"];[T "u3"]]
  [
    (["r1"] --> ["r2"]) [T "u1"];
    (["r2"] --> ["r3"]) [T "u2"];
    (["r3"] --> ["r1"]) [T "u3"];
  ]
  ["r1"]

let prod2 = product [s;r]
  [
    [T "t1"; Idle];
    [T "t2"; Idle];
    [T "t3"; T "u2"];
    [T "t4"; T "u2"];
    [T "t5"; Idle];
    [Idle; T "u1"];
    [Idle; T "u3"];
  ]
