(* Product of Fig. 3.5 p. 27 *)

open Unfoldings.Product_utils.StringPTNet

let r =
  of_lists [ "r1"; "r2"; "r3" ] [ "a0"; "c0" ]
    [ ([ "r1" ] --> [ "r2" ]) "a0"; ([ "r2" ] --> [ "r3" ]) "c0" ]
    [ "r1" ]

let s =
  of_lists [ "s1"; "s2"; "s3"; "s4" ] [ "a1"; "b1"; "c1" ]
    [
      ([ "s1" ] --> [ "s4" ]) "a1";
      ([ "s1" ] --> [ "s2" ]) "b1";
      ([ "s2" ] --> [ "s3" ]) "c1";
    ]
    [ "s1" ]

let t =
  of_lists [ "t1"; "t2"; "t3" ] [ "b2"; "c2" ]
    [ ([ "t1" ] --> [ "t2" ]) "b2"; ([ "t2" ] --> [ "t3" ]) "c2" ]
    [ "t1" ]

let u =
  of_lists [ "u1"; "u2"; "u3" ] [ "b3"; "c3" ]
    [ ([ "u1" ] --> [ "u2" ]) "b3"; ([ "u2" ] --> [ "u3" ]) "c3" ]
    [ "u1" ]

let v =
  of_lists [ "v1"; "v2"; "v3" ] [ "b4"; "c4" ]
    [ ([ "v1" ] --> [ "v2" ]) "b4"; ([ "v2" ] --> [ "v3" ]) "c4" ]
    [ "v1" ]

open Unfoldings.Product_utils.StringPTNetProduct

let prod3 =
  product [ r; s; t; u; v ]
    [
      [ T "a0"; T "a1"; Idle; Idle; Idle ];
      [ Idle; T "b1"; Idle; Idle; Idle ];
      [ Idle; Idle; T "b2"; Idle; Idle ];
      [ Idle; Idle; Idle; T "b3"; Idle ];
      [ Idle; Idle; Idle; Idle; T "b4" ];
      [ T "c0"; T "c1"; T "c2"; T "c3"; T "c4" ];
    ]
