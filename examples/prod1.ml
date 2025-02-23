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
  net {
    places: p1, p3;
    trans: t1, t3;

    t1: p1 --> p3;
    t3: p3 --> p1;

    tokens: p1;
  }
  net {
    places: p2, p4;
    trans: t1, t4;

    t2: p2 --> p4;
    t4: p4 --> p2;

    tokens: p2;
  }
} by {
  t1,   t2;
  t3,   idle;
  idle, t4;
}

*)
