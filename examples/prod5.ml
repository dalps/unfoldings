(* Product of Fig. 4.6 p. 59 *)

open Unfoldings.Lts

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

open Unfoldings.Product_pretrinet

let prod5 = product [s;t;u;v]
  [
    [Some "a1"; Some "a2"; Some "a3"; Some "a4"];
    [Some "b1"; Some "b2"; Some "b3"; Some "b4"];
    [Some "c1"; Some "c2"; None; None];
    [None; None; Some "d3"; Some "d4"];
    [Some "e1"; Some "e2"; None; None];
    [None; None; Some "f3"; Some "f4"];
    [Some "g1"; None; Some "g3"; None];
    [None; Some "h2"; None; Some "h4"];
    [Some "i1"; Some "i2"; Some "i3"; Some "i4"];
  ]
