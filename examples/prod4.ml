(* Product of Fig. 4.5 p. 54 *)

open Unfoldings.String_product.StringPTNet

let s =
  of_lists [ "s1"; "s2" ] [ "t1" ] [ ([ "s1" ] --> [ "s2" ]) "t1" ] [ "s1" ]

let r =
  of_lists [ "r1"; "r2"; "r3" ] [ "u1"; "u2" ]
    [ ([ "r1" ] --> [ "r2" ]) "u1"; ([ "r2" ] --> [ "r3" ]) "u2" ]
    [ "r1" ]

let q =
  of_lists [ "q1"; "q2" ] [ "v1" ] [ ([ "q1" ] --> [ "q2" ]) "v1" ] [ "q1" ]

open Unfoldings.String_product.StringPTNetProduct

let prod4 =
  product [ s; r; q ]
    [
      [ `T "t1"; `T "u1"; `Idle ];
      [ `Idle; `T "u1"; `T "v1" ];
      [ `T "t1"; `T "u2"; `Idle ];
      [ `Idle; `T "u2"; `T "v1" ];
    ]
