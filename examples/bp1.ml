(* Made up occurrence net *)

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
    ([s1;r1] --> [s2;r2]) e1;
    ([s2] --> [s3]) t1;
    ([r2] --> [r3]) u1;
    ([s3;r3] --> [s4;r4]) e2;
  ]
  [s1;r1]
