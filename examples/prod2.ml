(* Product of Fig. 2.2 p. 7 *)

open Unfoldings.Product_utils.StringPTNet

let s =
  of_lists [ "s1"; "s2"; "s3"; "s4" ]
    [ "t1"; "t2"; "t3"; "t4"; "t5" ]
    [
      ([ "s1" ] --> [ "s2" ]) "t1";
      ([ "s2" ] --> [ "s4" ]) "t3";
      ([ "s1" ] --> [ "s3" ]) "t2";
      ([ "s3" ] --> [ "s4" ]) "t4";
      ([ "s4" ] --> [ "s1" ]) "t5";
    ]
    [ "s1" ]

let r =
  of_lists [ "r1"; "r2"; "r3" ] [ "u1"; "u2"; "u3" ]
    [
      ([ "r1" ] --> [ "r2" ]) "u1";
      ([ "r2" ] --> [ "r3" ]) "u2";
      ([ "r3" ] --> [ "r1" ]) "u3";
    ]
    [ "r1" ]

open Unfoldings.Product_utils.StringPTNetProduct

let prod2 =
  product [ s; r ]
    [
      [ T "t1"; Idle ];
      [ T "t2"; Idle ];
      [ T "t3"; T "u2" ];
      [ T "t4"; T "u2" ];
      [ T "t5"; Idle ];
      [ Idle; T "u1" ];
      [ Idle; T "u3" ];
    ]
