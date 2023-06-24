(* Product of Fig. 4.6 p. 59 *)

open Unfoldings.Lts
open Unfoldings.Product_pretrinet

let s = product_component_of (build
  ["s1";"s2";"s3";"s4";"s5";"s6"]
  [
    ("s1" --> "s2") "a1";
    ("s1" --> "s3") "b1";
    ("s2" --> "s4") "c1";
    ("s3" --> "s4") "e1";
    ("s4" --> "s5") "g1";
    ("s5" --> "s6") "i1";
  ]
  "s1")

let t = product_component_of (build
  ["t1";"t2";"t3";"t4";"t5";"t6"]
  [
    ("t1" --> "t2") "a2";
    ("t1" --> "t3") "b2";
    ("t2" --> "t4") "c2";
    ("t3" --> "t4") "e2";
    ("t4" --> "t5") "h2";
    ("t5" --> "t6") "i2";
  ]
  "t1")

let u = product_component_of (build
  ["u1";"u2";"u3";"u4";"u5";"u6"]
  [
    ("u1" --> "u2") "a3";
    ("u1" --> "u3") "b3";
    ("u2" --> "u4") "d3";
    ("u3" --> "u4") "f3";
    ("u4" --> "u5") "g3";
    ("u5" --> "u6") "i3";
  ]
  "u1")

let v = product_component_of (build
  ["v1";"v2";"v3";"v4";"v5";"v6"]
  [
    ("v1" --> "v2") "a4";
    ("v1" --> "v3") "b4";
    ("v2" --> "v4") "d4";
    ("v3" --> "v4") "f4";
    ("v4" --> "v5") "h4";
    ("v5" --> "v6") "i4";
  ]
  "v1")

let prod5 = product [s;t;u;v]
  [
    [T "a1"; T "a2"; T "a3"; T "a4"];
    [T "b1"; T "b2"; T "b3"; T "b4"];
    [T "c1"; T "c2"; Idle; Idle];
    [Idle; Idle; T "d3"; T "d4"];
    [T "e1"; T "e2"; Idle; Idle];
    [Idle; Idle; T "f3"; T "f4"];
    [T "g1"; Idle; T "g3"; Idle];
    [Idle; T "h2"; Idle; T "h4"];
    [T "i1"; T "i2"; T "i3"; T "i4"];
  ]
