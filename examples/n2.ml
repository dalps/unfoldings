(* A net modelling a product of length 2, without cycles. 
   si are the states of transition system S,
   qi are the states of transition system Q,
   events where S and Q take a step singularly are denoted ti and ui respectively,
   events where S and Q synchronize are denoted ei. 
*)

open Unfoldings.Petrinet

let n2 = PetriNet.build
  ["s1";"s2";"s3";"s4";"r1";"r2";"r3";"r4"]
  ["t1";"u1";"e1";"e2"]
  [
    {source = (P "s1"); target = (E "e1")};
    {source = (P "r1"); target = (E "e1")};
    {source = (E "e1"); target = (P "s2")};
    {source = (E "e1"); target = (P "r2")};
    {source = (P "s2"); target = (E "t1")};
    {source = (P "r2"); target = (E "u1")};
    {source = (E "t1"); target = (P "s3")};
    {source = (E "u1"); target = (P "r3")};
    {source = (P "s3"); target = (E "e2")};
    {source = (P "r3"); target = (E "e2")};
    {source = (E "e2"); target = (P "s4")};
    {source = (E "e2"); target = (P "r4")}
  ]
  ["s1";"r1"]
