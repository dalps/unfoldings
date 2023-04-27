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
    Flow.to_event "s1" "e1";
    Flow.to_event "r1" "e1";
    Flow.to_place "e1" "s2";
    Flow.to_place "e1" "r2";
    Flow.to_event "s2" "t1";
    Flow.to_event "r2" "u1";
    Flow.to_place "t1" "s3";
    Flow.to_place "u1" "r3";
    Flow.to_event "s3" "e2";
    Flow.to_event "r3" "e2";
    Flow.to_place "e2" "s4";
    Flow.to_place "e2" "r4";
  ]
  ["s1";"r1"]
