(* Product of Fig. 2.2 p. 7 *)

open Stringnetlib.String_product.StringPTNet

let s =
  of_lists [ "k"; "l"; "m" ] [ "t1"; "t2"; "t3" ]
    [
      ([ "k" ] --> [ "l" ]) "t1";
      ([ "l" ] --> [ "m" ]) "t2";
      ([ "m" ] --> [ "k" ]) "t3";
    ]
    [ "k" ]

let r =
  of_lists [ "n"; "o"; "p" ] [ "u1"; "u2"; "u3" ]
    [
      ([ "n" ] --> [ "o" ]) "u1";
      ([ "o" ] --> [ "p" ]) "u2";
      ([ "p" ] --> [ "n" ]) "u3";
    ]
    [ "n" ]

open Stringnetlib.String_product.StringPTNetProduct

let prod8' =
  product [ s; r ] [ [ `T "t1"; `Idle ]; [ `T "t2"; `Idle ]; [ `Idle; `T "u1" ]; [ `Idle; `T "u2" ]; [ `T "t1" ; `T "u2" ]; [ `T "t3" ; `T "u3" ] ]
