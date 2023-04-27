(* A net modelling a product of length 2, without cycles. 
   si are the states of transition system S,
   qi are the states of transition system Q,
   events where S and Q take a step singularly are denoted ti and ui respectively,
   events where S and Q synchronize are denoted ei. 
*)

open Unfoldings.Petrinet

let n2 = build
  ["s1";"s2";"s3";"s4";"r1";"r2";"r3";"r4"]
  ["t1";"u1";"e1";"e2"]
  [
    "s1" @--> "e1";
    "r1" @--> "e1";
    "e1" -->@ "s2";
    "e1" -->@ "r2";
    "s2" @--> "t1";
    "r2" @--> "u1";
    "t1" -->@ "s3";
    "u1" -->@ "r3";
    "s3" @--> "e2";
    "r3" @--> "e2";
    "e2" -->@ "s4";
    "e2" -->@ "r4";
  ]
  ["s1";"r1"]
