open Unfoldings.Product_pretrinet
open PNet
open Flow

let s = of_lists
  ["s1";"s2";"s3";"s4"]
  ["t1";"t2";"t3";"t4";"t5"]
  [
    "s1" @--> "t1";
    "t1" -->@ "s2";
    "s2" @--> "t3";
    "t3" -->@ "s4";
    "s1" @--> "t2";
    "t2" -->@ "s3";
    "s3" @--> "t4";
    "t4" -->@ "s4";
    "s4" @--> "t5";
    "t5" -->@ "s1";    
  ]
  ["s1"]

let r = of_lists
  ["r1";"r2";"r3"]
  ["u1";"u2";"u3"]
  [
    "r1" @--> "u1";
    "u1" -->@ "r2";
    "r2" @--> "u2";
    "u2" -->@ "r3";
    "r3" @--> "u3";
    "u3" -->@ "r1";
  ]
  ["r1"]

let prod1 = product [s;r]
  [
    [Some "t1"; None];
    [Some "t2"; None];
    [Some "t3"; Some "u2"];
    [Some "t4"; Some "u2"];
    [Some "t5"; None];
    [None; Some "u1"];
    [None; Some "u3"];
  ]
