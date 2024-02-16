(* Product of Fig. 2.3 p. 9 *)

open Stringnetlib.String_product.StringPTNetProduct

let prod1 =
  of_lists [ "p1"; "p2"; "p3"; "p4" ]
    [ [ `T "t1" ]; [ `T "t2" ]; [ `T "t3" ] ]
    [
      ([ "p1"; "p2" ] --> [ "p3"; "p4" ]) [ `T "t2" ];
      ([ "p3" ] --> [ "p1" ]) [ `T "t1" ];
      ([ "p4" ] --> [ "p2" ]) [ `T "t3" ];
    ]
    [ "p1"; "p2" ]

(*
product {
  places: p1, p2, p3, p4;
  trans: t1, t2, t3;

  t2: p1, p2 --> p3, p4;
  t1: p3 --> p1;
  t3: p4 --> p2;
  
  tokens: p1, p2;
}

*)
