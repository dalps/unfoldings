(* Product of Fig. 2.2 p. 7 *)

open Modelchecklib.String_product.StringPTNet

let s =
  of_lists [ "a"; "b"; "c" ] [ "t1"; "t2" ]
    [ ([ "a" ] --> [ "b" ]) "t1"; ([ "b" ] --> [ "c" ]) "t2" ]
    [ "a" ]

let r = of_lists [ "m"; "n" ] [ "u1" ] [ ([ "m" ] --> [ "n" ]) "u1" ] [ "m" ]

open Modelchecklib.String_product.StringPTNetProduct

let prod9 =
  product [ s; r ]
    [ [ `T "t1"; `Idle ]; [ `T "t2"; `Idle ]; [ `T "t1"; `T "u1" ] ]
