(* Product of Fig. 3.5 p. 27 *)

open Unfoldings.Product
open Unfoldings.Product_transition

let r = of_lists
  ["r1";"r2";"r3"]
  [[T "a0"];[T "c0"]]
  [
    "r1" @--> [T "a0"];
    [T "a0"] -->@ "r2";
    "r2" @--> [T "c0"];
    [T "c0"] -->@ "r3"
  ]
  ["r1"]

let s = of_lists
  ["s1";"s2";"s3";"s4"]
  [[T "a1"];[T "b1"];[T "c1"]]
  [
    "s1" @--> [T "a1"];
    [T "a1"] -->@ "s4";
    "s1" @--> [T "b1"];
    [T "b1"] -->@ "s2";
    "s2" @--> [T "c1"];
    [T "c1"] -->@ "s3";
  ]
  ["s1"]

let t = of_lists
  ["t1";"t2";"t3"]
  [[T "b2"];[T "c2"]]
  [
    "t1" @--> [T "b2"];
    [T "b2"] -->@ "t2";
    "t2" @--> [T "c2"];
    [T "c2"] -->@ "t3";
  ]
  ["t1"]

let u = of_lists
  ["u1";"u2";"u3"]
  [[T "b3"];[T "c3"]]
  [
    "u1" @--> [T "b3"];
    [T "b3"] -->@ "u2";
    "u2" @--> [T "c3"];
    [T "c3"] -->@ "u3";
  ]
  ["u1"]

let v = of_lists
  ["v1";"v2";"v3"]
  [[T "b4"];[T "c4"]]
  [
    "v1" @--> [T "b4"];
    [T "b4"] -->@ "v2";
    "v2" @--> [T "c4"];
    [T "c4"] -->@ "v3";
  ]
  ["v1"]

let prod3 = product [r;s;t;u;v]
  [
    [T "a0"; T "a1"; Idle; Idle; Idle];
    [Idle; T "b1"; Idle; Idle; Idle];
    [Idle; Idle; T "b2"; Idle; Idle];
    [Idle; Idle; Idle; T "b3"; Idle];
    [Idle; Idle; Idle; Idle; T "b4"];
    [T "c0"; T "c1"; T "c2"; T "c3"; T "c4"]
  ]
