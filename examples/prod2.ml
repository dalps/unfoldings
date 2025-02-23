open Stringnetlib
open String_product

(* Product of Fig. 2.2 p. 7 *)

let prod2 =
  "product {
    net {
      places: s1, s2, s3, s4;
      transitions: t1, t2, t3, t4, t5;

      t1: s1 --> s2;
      t2: s1 --> s3;
      t3: s2 --> s4;
      t4: s3 --> s4;
      t5: s4 --> s1;

      tokens: s1
    }
    net {
      places: r1, r2, r3;
      transitions: u1, u2, u3;

      u1: r1 --> r2;
      u2: r2 --> r3;
      u3: r3 --> r1;

      tokens: r1
    }
  } by {
    t1, idle;
    t2, idle;
    t5, idle;
    t3, u2;
    t4, u2;
    idle, u1;
    idle, u3
  }
"
  |> Parser.parse |> List.hd
  |> function
  | `N _ -> failwith "no"
  | `P p -> p
