(* Product of Fig. 2.2 p. 7 *)

open Stringnetlib.String_product.StringPTNet

let s =
  of_lists [ "t1"; "t2"; ]
    [ "c" ]
    [
      ([ "t1" ] --> [ "t2" ]) "c";
    ]
    [ "t1" ]

let r =
  of_lists [ "u1"; "u2" ] [ "a"; "b" ]
    [
      ([ "u1" ] --> [ "u2" ]) "a";
      ([ "u2" ] --> [ "u1" ]) "b";
    ]
    [ "u1" ]

open Stringnetlib.String_product.StringPTNetProduct

let prod7 =
  product [ s; r ]
    [
      [ `T "c"; `Idle ];
      [ `Idle; `T "a" ];
      [ `Idle; `T "b" ];
    ]