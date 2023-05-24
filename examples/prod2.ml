(* Product of Fig. 3.5 p. 27 *)

open Unfoldings.Product_pretrinet
open PNet
open Flow

let r = of_lists
  ["r1";"r2";"r3"]
  ["a0";"c0"]
  [
    "r1" @--> "a0";
    "a0" -->@ "r2";
    "r2" @--> "c0";
    "c0" -->@ "r3"
  ]
  ["r1"]

let s = of_lists
  ["s1";"s2";"s3";"s4"]
  ["a1";"b1";"c1"]
  [
    "s1" @--> "a1";
    "a1" -->@ "s4";
    "s1" @--> "b1";
    "b1" -->@ "s2";
    "s2" @--> "c1";
    "c1" -->@ "s3";
  ]
  ["s1"]

let t = of_lists
  ["t1";"t2";"t3"]
  ["b2";"c2"]
  [
    "t1" @--> "b2";
    "b2" -->@ "t2";
    "t2" @--> "c2";
    "c2" -->@ "t3";
  ]
  ["t1"]

let u = of_lists
  ["u1";"u2";"u3"]
  ["b3";"c3"]
  [
    "u1" @--> "b3";
    "b3" -->@ "u2";
    "u2" @--> "c3";
    "c3" -->@ "u3";
  ]
  ["u1"]

let v = of_lists
  ["v1";"v2";"v3"]
  ["b4";"c4"]
  [
    "v1" @--> "b4";
    "b4" -->@ "v2";
    "v2" @--> "c4";
    "c4" -->@ "v3";
  ]
  ["v1"]

let prod2 = product [r;s;t;u;v]
  [
    [Some "a0"; Some "a1"; None; None; None];
    [None; Some "b1"; None; None; None];
    [None; None; Some "b2"; None; None];
    [None; None; None; Some "b3"; None];
    [None; None; None; None; Some "b4"];
    [Some "c0"; Some "c1"; Some "c2"; Some "c3"; Some "c4"]
  ]
