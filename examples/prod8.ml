(* Source: https://www.youtube.com/watch?v=AfgZFQ-EXkU&t=4s *)

open Stringnetlib.String_product.StringPTNetProduct

let prod8 =
  of_lists
    [ "k"; "l"; "m"; "n"; "o"; "p" ]
    [ [ `T "A" ]; [ `T "B" ]; [ `T "C" ]; [ `T "D" ]; [ `T "E" ]; [ `T "G" ] ]
    [
      ([ "k" ] --> [ "l" ]) [ `T "B" ];
      ([ "n" ] --> [ "o" ]) [ `T "D" ];
      ([ "k"; "o" ] --> [ "l"; "p" ]) [ `T "G" ];
      ([ "l" ] --> [ "m" ]) [ `T "C" ];
      ([ "o" ] --> [ "p" ]) [ `T "E" ];
      ([ "m"; "p" ] --> [ "k"; "n" ]) [ `T "A" ];
    ]
    [ "k"; "n" ]
