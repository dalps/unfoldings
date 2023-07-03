(* Product of Fig. 6.1 p. 99 *)

open Unfoldings.Product
open Unfoldings.Product.Trans

let prod6 = of_lists
  ["s1";"s2";"s3";"s4";"s5";"s6"]
  [[T "a"];[T "b"];[T "c"];[T "r"]]
  [
    (["s1"] --> ["s2"]) [T "a"];
    (["s1"] --> ["s3"]) [T "b"];
    (["s3"] --> ["s2"]) [T "c"];
    (["s2"] --> ["s3"]) [T "r"];
  ]
  ["s1"]