open Unfoldlib
open Petrilib
open Stringnetlib
open Examples.Prod1
open String_product.StringPTNetProduct
open Product

let _ = fire [ `T "t2" ] prod1

(* {p1,p2} ---> m1 := {p3,p4} *)
let _ = fire [ `T "t1" ] prod1

(* {p3,p4} ---> m2 := {p1,p4} *)
let _ = fire [ `T "t3" ] prod1

(* {p1,p4} ---> {p1,p2} = m0 *)
let _ = fire [ `T "t1" ] prod1

(* not enabled *)
let _ = fire [ `T "t9" ] prod1

(* not an event in n *)

let%test "" = (is_occurrence_sequence [ [ `T "t2" ]; [ `T "t1" ]; [ `T "t3" ] ] prod1)
let%test "" = (is_occurrence_sequence [ [ `T "t2" ]; [ `T "t3" ]; [ `T "t1" ] ] prod1)
let%test "" = (is_occurrence_sequence [ [ `T "t1" ] ] prod1 = false)

let%test "" = (
  is_occurrence_sequence [ [ `T "t2" ]; [ `T "t3" ]; [ `T "t2" ] ] prod1 = false)


let%test "" = (is_occurrence_sequence [ [ `T "t2" ]; [ `T "t2" ] ] prod1 = false)
(* --- end prod1 --- *)

(* --- *)

open Examples.Onet
open StringOccurrenceNet
open StringOccurrenceNet.Node

let%test "" = (is_occurrence_sequence [ e1; t1; u1; e2 ] onet)
let%test "" = (is_occurrence_sequence [ e1; u1; t1; e2 ] onet)
let%test "" = (is_occurrence_sequence [ e1; u1; e2 ] onet = false)
let%test "" = (is_occurrence_sequence [ e2; t1; u1; e1 ] onet = false)
let%test "" = (is_predecessor (of_trans e1) (of_trans e2) onet)
let%test "" = (is_predecessor (of_trans t1) (of_trans u1) onet = false)
let%test "" = (is_predecessor (of_place s2) (of_trans u1) onet = false)
let%test "" = (is_conflict (of_place s2) (of_place r2) onet = false)
let%test "" = (is_conflict (of_trans t1) (of_trans u1) onet = false)
let%test "" = (is_concurrent (of_trans t1) (of_trans u1) onet)
let%test "" = (is_concurrent (of_trans e1) (of_trans u1) onet = false)
let%test "" = (is_reachable (PlaceSet.of_list [ s1; r1 ]) onet)
let%test "" = (is_reachable (PlaceSet.of_list [ s2; r2 ]) onet)
let%test "" = (is_reachable (PlaceSet.of_list [ s3; r3 ]) onet)
let%test "" = (is_reachable (PlaceSet.of_list [ s4; r4 ]) onet)
let%test "" = (is_reachable (PlaceSet.of_list [ s4; r4; s3 ]) onet = false)
let%test "" = (is_reachable (PlaceSet.of_list [ s3; r2 ]) onet)
let%test "" = (is_reachable (PlaceSet.of_list [ r2; s3 ]) onet)
let%test "" = (is_reachable (PlaceSet.of_list [ r1; r2 ]) onet = false)
let%test "" = (is_reachable (PlaceSet.of_list [ r1 ]) onet)

(* questionable *)
let%test "" = (is_reachable (PlaceSet.of_list [ s1; s4 ]) onet = false)

let t2 = Event.build 5 [] "t2"
let t3 = Event.build 6 [] "t3"
let s2_4 = Token.build [] "s2_4"
let onet' = copy onet

let _ = add_trans t2 onet'
let _ = add_trans t3 onet'
let _ = add_place s2_4 onet'
let _ = add_to_trans_arc s2 t2 onet'
let _ = add_to_place_arc t2 s2_4 onet'
let _ = add_to_trans_arc s2_4 t3 onet'
let _ = add_to_place_arc t3 s4 onet'
let%test "" = (is_conflict (of_place s2_4) (of_place r3) onet' = false)
let%test "" = (is_conflict (of_place s2_4) (of_place s4) onet' = false)
let%test "" = (is_conflict (of_place s2_4) (of_place s3) onet')
let%test "" = (is_conflict (of_trans t1) (of_trans t2) onet')
let%test "" = (is_conflict (of_trans e2) (of_place s2_4) onet')
let%test "" = (is_conflict (of_trans t3) (of_trans e2) onet')
let%test "" = (is_conflict (of_trans t2) (of_trans e2) onet')
let%test "" = (is_concurrent (of_trans t3) (of_trans u1) onet')
let%test "" = (is_concurrent (of_trans t2) (of_trans u1) onet')
let%test "" = (is_concurrent (of_trans t3) (of_trans e2) onet' = false)
let%test "" = (is_concurrent (of_trans t2) (of_trans e2) onet' = false)
let%test "" = (is_concurrent (of_trans t3) (of_trans t1) onet' = false)
(* --- end onet --- *)

(* --- *)

open Examples.Prod2
open String_product.StringPTNetProduct
open String_product

let t1 = [ `T "t1"; `Idle ]
let t2 = [ `T "t2"; `Idle ]
let u1 = [ `Idle; `T "u1" ]
let t3u2 = [ `T "t3"; `T "u2" ]
let t4u2 = [ `T "t4"; `T "u2" ]
let t4 = [ `T "t4"; `Idle ]
let t5 = [ `T "t5"; `Idle ]
let u3 = [ `Idle; `T "u3" ]

let%test "" = (is_occurrence_sequence [ t1; u1; t3u2; t5 ] prod2)
let%test "" = (is_occurrence_sequence [ u1; t1; t3u2; u3 ] prod2)
let%test "" = (is_occurrence_sequence [ u1; t3u2 ] prod2 = false)
let%test "" = (is_occurrence_sequence [ t1; u1; t4u2; u3 ] prod2 = false)
let%test "" = (is_occurrence_sequence [ t1; u1; t4u2; u3 ] prod2 = false)
let%test "" = (is_occurrence_sequence [ t2; u1; t4u2; u3 ] prod2)
let%test "" = (is_occurrence_sequence [ u1; t2; t4u2; u3; t5 ] prod2)
let%test "" = (is_occurrence_sequence [ u1; t2; t4u2; t5; u3; u1 ] prod2)
let%test "" = (is_occurrence_sequence [ u1; t2; t4u2; t5; u3; u1; t4u2 ] prod2 = false)

let%test "" = (is_occurrence_sequence [ u1; t2; t4u2; t5; u3; t2; u1; t4u2 ] prod2)
let%test "" = (is_occurrence_sequence [ u1; t2; t4u2; t5; u3; t1; u1; t3u2 ] prod2)
let%test "" = (is_independent t1 u1)
let%test "" = (is_independent t1 t5 = false)
let%test "" = (is_independent t3u2 t4u2 = false)
let%test "" = (tword_equiv [ t1; u1; t3u2; t5; u3 ] [ u1; t1; t3u2; u3; t5 ])
let%test "" = (tword_equiv [ t1; u1; t3u2; t5; u3 ] [ u1; t1; t3u2; t5; u3 ])
let%test "" = (tword_equiv [ u1; t1; t3u2; t5; u3 ] [ u1; t1; t3u2; u3; t5 ])
let%test "" = (tword_equiv [ u1; t1; t3u2; t5; u3 ] [ u1; t1; t3u2; t5; u3 ])
let%test "" = (tword_equiv [ u1; t1; t3u2; t5; u3 ] [ u1; t1; t3u2; t4; u3 ] = false)
let%test "" = (tword_equiv [ u1; t1; t3u2; t5; u3 ] [ u1; t1; t3u2; t4 ] = false)
let%test "" = (tword_equiv [ u1; t1; t3u2; t5; u3 ] [ u1; t1; t3u2; t5; t4u2 ] = false)

let a = [ `T "a"; `Idle; `Idle ]
let b = [ `Idle; `T "b"; `Idle ]
let c = [ `Idle; `Idle; `T "c" ]
let c1 = [ `Idle; `Idle; `T "c1" ]

let%test "" = (tword_equiv [ a; b; c ] [ b; a; c ])
let%test "" = (tword_equiv [ a; b; c ] [ a; c; b ])
let%test "" = (tword_equiv [ b; a; c ] [ a; c; b ])
let%test "" = (tword_equiv [ a; b; c ] [ a; c1; b ] = false)
let%test "" = (tword_equiv [ a; b; c ] [ c; b; a ] = false)
(* --- end prod2 --- *)

(* --- *)

let d = [ `T "d"; `Idle; `Idle ]
let ef = [ `Idle; `T "e"; `T "f" ]
let gc = [ `T "g"; `Idle; `T "c" ]
let t4u4 = [ `T "t4"; `T "u4" ]
let h1 = [ t1; u1; t3u2; t5; u3 ]
let h21 = [ a; b; c ]
let h22 = [ d; ef; gc ]
let h3 = [ t1; u1; t5; u3; t1; u1; t3u2; t5; u3; t4u4 ]
let h4 = [ [ `T "t" ] ]
let traceh3 = trace h3
let traceh21 = trace h21
let traceh22 = trace h22

let test_trace_equiv t =
  List.fold_left
    (fun b v -> List.fold_left (fun c w -> tword_equiv w v && c) b t)
    true t

let test_trace_proj i t =
  List.fold_left
    (fun b v ->
      List.fold_left (fun c w -> projection i w = projection i v && c) b t)
    true t

let naive_concat ws vs =
  List.fold_left
    (fun traces w ->
      List.fold_left (fun traces v -> [ w @ v ] @ traces) [] vs @ traces)
    [] ws


let%test "" = (test_trace_equiv (trace h1))
let%test "" = (test_trace_equiv (trace h21))
let%test "" = (test_trace_equiv traceh22)
let%test "" = (test_trace_equiv traceh3)
let%test "" = (test_trace_equiv (trace h4))
let%test "" = (test_trace_proj 0 traceh3)
let%test "" = (test_trace_proj 1 traceh3)
let%test "" = (test_trace_proj 0 traceh21)
let%test "" = (test_trace_proj 1 traceh21)
let%test "" = (test_trace_proj 2 traceh21)
let%test "" = (test_trace_proj 0 traceh22)
let%test "" = (test_trace_proj 1 traceh22)
let%test "" = (test_trace_proj 2 traceh22)
let%test "" = (naive_concat (trace h1) (trace h3) <> trace (h1 @ h3))

(* rhs is larger *)
let%test "" = (naive_concat (trace h21) (trace h22) <> trace (h21 @ h22))

(* rhs is larger *)

(* --- *)

let e1 = [ u1 ]
let e2 = [ t1 ]
let e6 = [ u1; t1; t3u2; u3; u1 ]
let e7 = [ t2; u1; t4u2 ]
let h1 = [ u1; t1 ]
let h2 = [ t1; u1 ]

let%test "" = (d_compare sl_compare e1 e2 < 0)
let%test "" = (d_compare sl_compare e1 e2 < 0)
let%test "" = (d_compare sl_compare e6 e7 < 0)
let%test "" = (d_compare sl_compare e1 e6 < 0)
let%test "" = (d_compare sl_compare e2 e6 < 0)
let%test "" = (d_compare sl_compare e1 e7 < 0)
let%test "" = (d_compare sl_compare e2 e7 < 0)
let%test "" = (d_compare sl_compare h1 h2 = 0)
(* --- end misc --- *)

(* --- *)

open Examples
open Examples.Prod1
open Examples.Prod2
open Examples.Prod3
open Examples.Prod4
open Examples.Prod5
open Examples.Prod5_loops
open String_product

let%test "" = (StringPTNetProduct.is_freechoice prod1)
let%test "" = (StringPTNetProduct.is_freechoice prod2 = false)
let%test "" = (StringPTNetProduct.is_freechoice prod3 = false)
let%test "" = (StringPTNetProduct.is_freechoice prod4 = false)
let%test "" = (StringPTNetProduct.is_freechoice prod5 = false)
let%test "" = (StringPTNetProduct.is_freechoice prod5' = false)

module Exe = Executability.Make (StringPTNetProduct)
open Exe

let fc1 = Unfolder.(unfold 10 prod1)

let%test "" = (Unfolder.OccurrenceNet.is_freechoice fc1 = false)
(* --- end is_freechoice --- *)
let%test "" = (let r = Exe.test prod1 (d_compare sl_compare) [ [ `T "t1" ] ] 99 in r.res)
let%test "" = (let r = Exe.test prod1 (d_compare sl_compare) [ [ `T "t2" ] ] 99 in r.res)
let%test "" = (let r = Exe.test prod1 (d_compare sl_compare) [ [ `T "t3" ] ] 99 in r.res)
(* --- end is_executable prod1 --- *)
let%test "" = (let r = Exe.test prod2 (d_compare sl_compare) [ u3 ] 99 in r.res)
let%test "" = (let r = Exe.test prod2 (d_compare sl_compare) [ t4u2 ] 99 in r.res)
let%test "" = (let r = Exe.test prod2 (d_compare sl_compare) [ t5 ] 99 in r.res)
let%test "" = (let r = Exe.test prod2 (d_compare sl_compare) [ t3u2 ] 99 in r.res)
(* --- end is_executable prod2 --- *)

let a0a1 = [ `T "a0"; `T "a1"; `Idle; `Idle; `Idle ]
let b1 = [ `Idle; `T "b1"; `Idle; `Idle; `Idle ]
let b2 = [ `Idle; `Idle; `T "b2"; `Idle; `Idle ]
let b3 = [ `Idle; `Idle; `Idle; `T "b3"; `Idle ]
let b4 = [ `Idle; `Idle; `Idle; `Idle; `T "b4" ]
let c = [ `T "c0"; `T "c1"; `T "c2"; `T "c3"; `T "c4" ]

let%test "" = (let r = Exe.test prod3 (d_compare sl_compare) [ b1 ] 99 in r.res)
let%test "" = (let r = Exe.test prod3 (d_compare sl_compare) [ b2 ] 99 in r.res)
let%test "" = (let r = Exe.test prod3 (d_compare sl_compare) [ b3 ] 99 in r.res)
let%test "" = (let r = Exe.test prod3 (d_compare sl_compare) [ b4 ] 99 in r.res)
let%test "" = (let r = Exe.test prod3 (d_compare sl_compare) [ a0a1 ] 99 in r.res)
let%test "" = (let r = Exe.test prod3 (d_compare sl_compare) [ c ] 99 in r.res = false)
let%test "" = (let r = Exe.test prod3 (d_compare sl_compare) [ c; a0a1 ] 99 in r.res)
(* --- end is_executable prod3 --- *)
let%test "" = (let r = Exe.test prod4 (d_compare sl_compare) [ [ `Idle; `T "u1"; `T "v1" ] ] 99 in r.res)
let%test "" = (let r = Exe.test prod4 (d_compare sl_compare) [ [ `Idle; `T "u1"; `Idle ] ] 99 in r.res = false)

let%test "" = (let r = Exe.test prod4 (d_compare sl_compare) [ [ `T "t1"; `T "u1"; `Idle ] ] 99 in r.res)
(* --- end is_executable prod4 --- *)
let%test "" = (let r = Exe.test prod5 (d_compare sl_compare) [ [ `Idle; `Idle; `T "f3"; `T "f4" ] ] 99 in r.res)


let%test "" = (
  let r = Exe.test prod5 (d_compare sl_compare) [ [ `Idle; `Idle; `T "f3"; `T "f5" ] ] 99 in r.res
  = false)


let%test "" = (
  let r = Exe.test prod5 (d_compare sl_compare) [ [ `T "i1"; `T "i2"; `T "i3"; `T "i4" ] ] 99 in r.res)


let%test "" = (let r = Exe.test prod5 sl_compare [ [ `T "i1"; `T "i2"; `T "i3"; `T "i4" ] ] 99 in r.res)
(* --- end is_executable prod5 --- *)

let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [`T "A"] ] 99 in r.res) 
let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [`T "B"] ] 99 in r.res) 
let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [`T "C"] ] 99 in r.res) 
let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [`T "D"] ] 99 in r.res) 
let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [`T "E"] ] 99 in r.res) 
let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [`T "G"] ] 99 in r.res) 
(* --- end is_executable prod8 --- *)

(* --- *)

module Rep = Repeated_executability.Make (StringPTNetProduct)
open Examples.Prod5_loops
open Examples.Prod6

let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "A" ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "B" ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "C" ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "D" ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "E" ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "G" ] ] 99 in r.res) 

let _ = StringPTNetProduct.add_trans [ `T "F" ] Prod8.prod8
 
let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [ `T "F" ] ] 99 in r.res = false) 
let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "F" ] ] 99 in r.res = false) 

let _ = StringPTNetProduct.add_to_trans_arc "o" [ `T "F" ] Prod8.prod8

let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [ `T "F" ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "F" ] ] 99 in r.res = false) 

let _ = StringPTNetProduct.add_to_place_arc [ `T "F" ] "n" Prod8.prod8

let%test "" = (let r = Exe.test Prod8.prod8 (d_compare sl_compare) [ [ `T "F" ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod8.prod8 (d_compare sl_compare) [ [ `T "F" ] ] 99 in r.res) 

(* --- end is_infinitely_executable prod8 --- *)

let%test "" = (let r = Exe.test Prod7.prod7 (d_compare sl_compare) [ [ `T "c"; `Idle ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod7.prod7 (d_compare sl_compare) [ [ `T "c"; `Idle ] ] 99 in r.res = false) 
let%test "" = (let r = Rep.test Prod7.prod7 (d_compare sl_compare) [ [ `Idle; `T "a"  ] ] 99 in r.res) 
let%test "" = (let r = Rep.test Prod7.prod7 (d_compare sl_compare) [ [ `Idle; `T "b"  ] ] 99 in r.res) 

(* --- end is_infinitely_executable prod7 --- *)

let%test "" = (let r = Rep.test prod6 (d_compare sl_compare) [ [ `T "r" ] ] 99 in r.res)
let%test "" = (let r = Rep.test prod6 (d_compare sl_compare) [ [ `T "c" ] ] 99 in r.res)
let%test "" = (let r = Rep.test prod6 (d_compare sl_compare) [ [ `T "a" ] ] 99 in r.res = false)
let%test "" = (let r = Rep.test prod6 (d_compare sl_compare) [ [ `T "b" ] ] 99 in r.res = false)
(* --- end is_infinitely_executable prod6 --- *)
let%test "" = (let r = Rep.test prod1 (d_compare sl_compare) [ [ `T "t1" ] ] 99 in r.res)
let%test "" = (let r = Rep.test prod1 (d_compare sl_compare) [ [ `T "t2" ] ] 99 in r.res)
let%test "" = (let r = Rep.test prod1 (d_compare sl_compare) [ [ `T "t3" ] ] 99 in r.res)
(* --- end is_infinitely_executable prod1 --- *)
let%test "" = (let r = Rep.test prod2 (d_compare sl_compare) [ u3 ] 99 in r.res)
let%test "" = (let r = Rep.test prod2 (d_compare sl_compare) [ t4u2 ] 99 in r.res)
let%test "" = (let r = Rep.test prod2 (d_compare sl_compare) [ t5 ] 99 in r.res)
let%test "" = (let r = Rep.test prod2 (d_compare sl_compare) [ t3u2 ] 99 in r.res)
(* --- end is_infinitely_executable prod2 --- *)

let a0a1 = [ `T "a0"; `T "a1"; `Idle; `Idle; `Idle ]
let b1 = [ `Idle; `T "b1"; `Idle; `Idle; `Idle ]
let b2 = [ `Idle; `Idle; `T "b2"; `Idle; `Idle ]
let b3 = [ `Idle; `Idle; `Idle; `T "b3"; `Idle ]
let b4 = [ `Idle; `Idle; `Idle; `Idle; `T "b4" ]
let c = [ `T "c0"; `T "c1"; `T "c2"; `T "c3"; `T "c4" ]

let%test "" = (let r = Rep.test prod3 (d_compare sl_compare) [ b1 ] 99 in r.res = false)
let%test "" = (let r = Rep.test prod3 (d_compare sl_compare) [ b2 ] 99 in r.res = false)
let%test "" = (let r = Rep.test prod3 (d_compare sl_compare) [ b3 ] 99 in r.res = false)
let%test "" = (let r = Rep.test prod3 (d_compare sl_compare) [ b4 ] 99 in r.res = false)
let%test "" = (let r = Rep.test prod3 (d_compare sl_compare) [ a0a1 ] 99 in r.res = false)
let%test "" = (let r = Rep.test prod3 (d_compare sl_compare) [ c ] 99 in r.res = false)
let%test "" = (let r = Rep.test prod3 (d_compare sl_compare) [ c; a0a1 ] 99 in r.res = false)
(* --- end is_infinitely_executable prod3 --- *)

let%test "" = (
  let r = Rep.test prod4 (d_compare sl_compare) [ [ `Idle; `T "u1"; `T "v1" ] ] 99 in r.res = false)


let%test "" = (let r = Rep.test prod4 (d_compare sl_compare) [ [ `Idle; `T "u1"; `Idle ] ] 99 in r.res = false)


let%test "" = (
  let r = Rep.test prod4 (d_compare sl_compare) [ [ `T "t1"; `T "u1"; `Idle ] ] 99 in r.res = false)


(* --- end is_infinitely_executable prod4 --- *)

let%test "" = (
  let r = Rep.test prod5 (d_compare sl_compare) [ [ `Idle; `Idle; `T "f3"; `T "f4" ] ] 99 in r.res
  = false)


let%test "" = (
  let r = Rep.test prod5 (d_compare sl_compare) [ [ `Idle; `Idle; `T "f3"; `T "f5" ] ] 99 in r.res
  = false)


let%test "" = (
  let r = Rep.test prod5 (d_compare sl_compare) [ [ `T "i1"; `T "i2"; `T "i3"; `T "i4" ] ] 99 in r.res
  = false)


let%test "" = (let r = Rep.test prod5 sl_compare [ [ `T "i1"; `T "i2"; `T "i3"; `T "i4" ] ] 99 in r.res = false)

(* --- end is_infinitely_executable prod5 --- *)

let%test "" = (
  let r = Rep.test prod5' (d_compare sl_compare) [ [ `Idle; `Idle; `T "f3"; `T "f4" ] ] 99 in r.res
  = false)


let%test "" = (
  let r = Rep.test prod5' (d_compare sl_compare) [ [ `Idle; `Idle; `T "f3"; `T "f5" ] ] 99 in r.res
  = false)


let%test "" = (
  let r = Rep.test prod5' (d_compare sl_compare) [ [ `T "i1"; `T "i2"; `T "i3"; `T "i4" ] ] 99 in r.res)


let%test "" = (let r = Rep.test prod5' sl_compare [ [ `T "i1"; `T "i2"; `T "i3"; `T "i4" ] ] 99 in r.res)
(* --- end is_infinitely_executable prod5_loops --- *)

open Examples.Onet
open StringOccurrenceNet

let m0 = marking rev_onet

let%test "" = (PlaceSet.equal (places rev_onet) (places onet))

let%test "" = (
  TransSet.cardinal (transitions rev_onet)
  = 2 * TransSet.cardinal (transitions onet))


let%test "" = (TransSet.subset (transitions onet) (transitions rev_onet))
let%test "" = (PlaceSet.equal (marking rev_onet) (marking onet))

let _ = TransSet.iter
  (fun t ->
    assert (TransSet.mem (`Rev t) (transitions rev_onet));
    assert (PlaceSet.equal (preset_t rev_onet (`Rev t)) (postset_t rev_onet t));
    assert (PlaceSet.equal (postset_t rev_onet (`Rev t)) (preset_t rev_onet t)))
  (transitions onet)


let%test "" = (Event.compare (`Rev (`Rev e1)) e1 = 0)
let%test "" = (Event.compare (`Rev (`Rev e1)) e1 = Event.compare e1 e1)
let%test "" = (Event.compare (`Rev (`Rev e1)) e2 = Event.compare e1 e2)
let%test "" = (Event.compare (`Rev e1) e1 = 1)
let%test "" = (Event.compare (`Rev e1) (`Rev e2) = Event.compare e1 e2)
let%test "" = (Event.compare (`Rev e1) (`Rev e2) = Event.compare e1 e2)
let%test "" = (PlaceSet.equal (preset_t rev_onet (`Rev (`Rev e1))) (preset_t rev_onet e1))


let%test "" = (
  PlaceSet.equal
    (preset_t rev_onet (`Rev (`Rev (`Rev e2))))
    (preset_t rev_onet (`Rev e2)))


let%test "" = (
  PlaceSet.equal
    (postset_t rev_onet (`Rev e2))
    (postset_t rev_onet (`Rev (`Rev (`Rev e2)))))


let _ = fire_sequence [ e1; `Rev e1 ] rev_onet
let%test "" = (PlaceSet.equal (marking rev_onet) m0)
let _ = fire (`Rev e1) rev_onet
let%test "" = (PlaceSet.equal (marking rev_onet) m0)
let _ = fire_sequence [ e1; t1; u1; e2; `Rev e2; `Rev t1; `Rev u1; `Rev e1 ] rev_onet
let%test "" = (PlaceSet.equal (marking rev_onet) m0)
(* --- end reversible --- *)
