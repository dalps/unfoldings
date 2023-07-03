(* Product of Fig. 4.6 p. 59, but every transition is turned into a self loop *)

open Unfoldings.Product
open Unfoldings.Product.GlobalT

let s =
  of_lists
    [ "s1"; "s2"; "s3"; "s4"; "s5" ]
    [ [ T "a1" ]; [ T "b1" ]; [ T "c1" ]; [ T "e1" ]; [ T "g1" ]; [ T "i1" ] ]
    [
      ([ "s1" ] --> [ "s2" ]) [ T "a1" ];
      ([ "s1" ] --> [ "s3" ]) [ T "b1" ];
      ([ "s2" ] --> [ "s4" ]) [ T "c1" ];
      ([ "s3" ] --> [ "s4" ]) [ T "e1" ];
      ([ "s4" ] --> [ "s5" ]) [ T "g1" ];
      ([ "s5" ] --> [ "s5" ]) [ T "i1" ];
    ]
    [ "s1" ]

let t =
  of_lists
    [ "t1"; "t2"; "t3"; "t4"; "t5" ]
    [ [ T "a2" ]; [ T "b2" ]; [ T "c2" ]; [ T "e2" ]; [ T "h2" ]; [ T "i2" ] ]
    [
      ([ "t1" ] --> [ "t2" ]) [ T "a2" ];
      ([ "t1" ] --> [ "t3" ]) [ T "b2" ];
      ([ "t2" ] --> [ "t4" ]) [ T "c2" ];
      ([ "t3" ] --> [ "t4" ]) [ T "e2" ];
      ([ "t4" ] --> [ "t5" ]) [ T "h2" ];
      ([ "t5" ] --> [ "t5" ]) [ T "i2" ];
    ]
    [ "t1" ]

let u =
  of_lists
    [ "u1"; "u2"; "u3"; "u4"; "u5" ]
    [ [ T "a3" ]; [ T "b3" ]; [ T "d3" ]; [ T "f3" ]; [ T "g3" ]; [ T "i3" ] ]
    [
      ([ "u1" ] --> [ "u2" ]) [ T "a3" ];
      ([ "u1" ] --> [ "u3" ]) [ T "b3" ];
      ([ "u2" ] --> [ "u4" ]) [ T "d3" ];
      ([ "u3" ] --> [ "u4" ]) [ T "f3" ];
      ([ "u4" ] --> [ "u5" ]) [ T "g3" ];
      ([ "u5" ] --> [ "u5" ]) [ T "i3" ];
    ]
    [ "u1" ]

let v =
  of_lists
    [ "v1"; "v2"; "v3"; "v4"; "v5" ]
    [ [ T "a4" ]; [ T "b4" ]; [ T "d4" ]; [ T "f4" ]; [ T "h4" ]; [ T "i4" ] ]
    [
      ([ "v1" ] --> [ "v2" ]) [ T "a4" ];
      ([ "v1" ] --> [ "v3" ]) [ T "b4" ];
      ([ "v2" ] --> [ "v4" ]) [ T "d4" ];
      ([ "v3" ] --> [ "v4" ]) [ T "f4" ];
      ([ "v4" ] --> [ "v5" ]) [ T "h4" ];
      ([ "v5" ] --> [ "v5" ]) [ T "i4" ];
    ]
    [ "v1" ]

let prod5' =
  product [ s; t; u; v ]
    [
      [ T "a1"; T "a2"; T "a3"; T "a4" ];
      [ T "b1"; T "b2"; T "b3"; T "b4" ];
      [ T "c1"; T "c2"; Idle; Idle ];
      [ Idle; Idle; T "d3"; T "d4" ];
      [ T "e1"; T "e2"; Idle; Idle ];
      [ Idle; Idle; T "f3"; T "f4" ];
      [ T "g1"; Idle; T "g3"; Idle ];
      [ Idle; T "h2"; Idle; T "h4" ];
      [ T "i1"; T "i2"; T "i3"; T "i4" ];
    ]
