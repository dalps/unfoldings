(* Made up occurrence net *)

module StringOccurrenceNet =
  Petrilib.Occurrence_net.Make (String) (String)
(* do NOT open Make directly, always wrap in another module *)

open StringOccurrenceNet

let t1 = Event.build 1 [] "t1"
let u1 = Event.build 2 [] "u1"
let e1 = Event.build 3 [] "e1"
let e2 = Event.build 4 [] "e2"
let s1 = Token.build [] "s1"
let s2 = Token.build [] "s2"
let s3 = Token.build [] "s3"
let s4 = Token.build [] "s4"
let r1 = Token.build [] "r1"
let r2 = Token.build [] "r2"
let r3 = Token.build [] "r3"
let r4 = Token.build [] "r4"

let onet =
  of_lists
    [ s1; s2; s3; s4; r1; r2; r3; r4 ]
    [ t1; u1; e1; e2 ]
    [
      ([ s1; r1 ] --> [ s2; r2 ]) e1;
      ([ s2 ] --> [ s3 ]) t1;
      ([ r2 ] --> [ r3 ]) u1;
      ([ s3; r3 ] --> [ s4; r4 ]) e2;
    ]
    [ s1; r1 ]

let rev_onet = reversible onet
