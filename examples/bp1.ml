(* A Petri net modelling the unfolding of a product of two components S and Q. 
   si are the states of transition system S, qi are the states of transition 
   system Q. Events where S and Q take a step singularly are denoted ti and ui 
   respectively, while events where S and Q synchronize are denoted ei.
*)

open Unfoldings.Branching_process

let t1 = Event.build 1 [] [T "t1"]
let u1 = Event.build 2 [] [T "u1"]
let e1 = Event.build 3 [] [T "e1"]
let e2 = Event.build 4 [] [T "e2"]

let s1 = LabelledPlace.build [] "s1"
let s2 = LabelledPlace.build [] "s2"
let s3 = LabelledPlace.build [] "s3"
let s4 = LabelledPlace.build [] "s4"
let r1 = LabelledPlace.build [] "r1"
let r2 = LabelledPlace.build [] "r2"
let r3 = LabelledPlace.build [] "r3"
let r4 = LabelledPlace.build [] "r4"

let bp1 = of_lists
  [s1;s2;s3;s4;r1;r2;r3;r4]
  [t1;u1;e1;e2]
  [
    s1 @--> e1;
    r1 @--> e1;
    e1 -->@ s2;
    e1 -->@ r2;
    s2 @--> t1;
    r2 @--> u1;
    t1 -->@ s3;
    u1 -->@ r3;
    s3 @--> e2;
    r3 @--> e2;
    e2 -->@ s4;
    e2 -->@ r4;
  ]
  [s1;r1]
