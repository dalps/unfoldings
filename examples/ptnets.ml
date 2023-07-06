(* P/T nets of Fig. 2 p. 5 (HernanClaudioIrek) *)

open Unfoldings.Petrinet.Make (String) (String)

let o1 =
  of_lists [ "a"; "b"; "c"; "d" ] [ "t1"; "t2" ]
    [ ([ "a" ] --> [ "c" ]) "t1"; ([ "b" ] --> [ "d" ]) "t2" ]
    [ "a"; "b" ]

let n1 =
  of_lists
    [ "a"; "b"; "c"; "d"; "e"; "f" ]
    [ "t1"; "t2"; "t3" ]
    [
      ([ "a" ] --> [ "c" ]) "t1";
      ([ "b"; "c" ] --> [ "e" ]) "t2";
      ([ "c"; "d" ] --> [ "f" ]) "t3";
    ]
    [ "a"; "b"; "c"; "d" ]

let n2 =
  of_lists
    [ "a"; "b"; "c"; "d"; "e" ]
    [ "t1"; "t2"; "t3" ]
    [
      ([ "a" ] --> [ "d" ]) "t1";
      ([ "b" ] --> [ "d" ]) "t2";
      ([ "c"; "d" ] --> [ "e" ]) "t3";
    ]
    [ "a"; "b" ]

let n3 =
  of_lists [ "a"; "b" ] [ "t1" ] [ ([ "a" ] --> [ "a"; "b" ]) "t1" ] [ "a" ]

let n4 =
  of_lists
    [ "a"; "b"; "c"; "d"; "e" ]
    [ "t1"; "t2" ]
    [
      ([ "a"; "c" ] --> [ "c"; "d" ]) "t1"; ([ "b"; "c" ] --> [ "c"; "e" ]) "t2";
    ]
    [ "a"; "b"; "c" ]
