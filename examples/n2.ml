(* A net modelling a product of length 2, without cycles. 
   si are the states of transition system S,
   qi are the states of transition system Q,
   events where S and Q take a step singularly are denoted ti and ui respectively,
   events where S and Q synchronize are denoted ei. 
*)

open Unfoldings.Event
open Unfoldings.Branching_process.BPNet
open Flow

let t1 = build 1 "t1"
let u1 = build 2 "u1"
let e1 = build 3 "e1"
let e2 = build 4 "e2"

open Unfoldings.Labelled_place

let s1 = build 1 "s1"
let s2 = build 2 "s2"
let s3 = build 3 "s3"
let s4 = build 4 "s4"
let r1 = build 5 "r1"
let r2 = build 6 "r2"
let r3 = build 7 "r3"
let r4 = build 8 "r4"

let n2 = of_lists
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
