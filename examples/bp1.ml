(* A Petri net modelling the unfolding of a product of two components S and Q. 
   si are the states of transition system S, qi are the states of transition 
   system Q. Events where S and Q take a step singularly are denoted ti and ui 
   respectively, while events where S and Q synchronize are denoted ei.
*)

open Unfoldings.Event
open Unfoldings.Branching_process.BPNet
open Flow

let t1 = build 1 [] "t1"
let u1 = build 2 [] "u1"
let e1 = build 3 [] "e1"
let e2 = build 4 [] "e2"

open Unfoldings.Labelled_place

let s1 = build [] "s1"
let s2 = build [] "s2"
let s3 = build [] "s3"
let s4 = build [] "s4"
let r1 = build [] "r1"
let r2 = build [] "r2"
let r3 = build [] "r3"
let r4 = build [] "r4"

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
