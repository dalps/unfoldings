print_endline "#### Running ltl tests..."

open Unfoldings
open Examples
module StringLtl = Unfoldings.Ltl.Make (String)
open Ltl
open StringLtl;;

assert (length (X (AP "a")) = 1);;
assert (length (And (X (AP "a"), AP "b")) = 2);;
assert (length (U (X (AP "a"), AP "b")) = 2);;
assert (length (U (X (AP "a"), And (AP "a", AP "b"))) = 3);;
assert (length (U (X (AP "a"), And (AP "a", Not (AP "b")))) = 4);;
print_endline "[OK] length"

let ab = APSet.of_list [ "a"; "b" ]
let a = APSet.of_list [ "a" ]
let b = APSet.of_list [ "b" ]
let empty = APSet.empty;;

assert (eval [ ab; a; a ] (And (AP "a", AP "b")));;
assert (eval [ ab; a; a ] (And (AP "a", X (AP "b"))) = false);;
assert (eval [ ab; a; a ] (And (AP "a", X (AP "a"))));;
assert (eval [ ab; empty; b ] (X (AP "a")) = false);;
assert (eval [ ab; a; a ] (U (AP "a", X (AP "a"))));;
assert (eval [ ab; a; a ] (And (AP "a", Not (X (AP "a")))) = false);;
assert (eval [ ab; a; a; b ] (U (AP "a", AP "b")));;
assert (eval [ ab; a; a; b ] (U (AP "a", And (AP "b", AP "a"))));;
assert (eval [ ab ] (U (AP "a", And (AP "b", AP "a"))));;
assert (eval [ a; a; a; ab ] (U (AP "a", And (AP "b", AP "a"))));;
assert (eval [ a; a; a; b ] (U (AP "a", And (AP "b", AP "a"))) = false);;
assert (eval [ a; ab; a; ab ] (U (AP "a", And (AP "b", AP "a"))));;
assert (eval [ a; b; a; ab ] (U (AP "a", And (AP "b", AP "a"))) = false);;
assert (eval [ empty; ab; a; ab ] (U (AP "a", And (AP "b", AP "a"))) = false);;
assert (eval [ a; a; a; ab; a ] (U (True, And (AP "b", AP "a"))));;
assert (eval [ a; a; a; ab ] (U (True, And (AP "b", X (AP "a")))) = false);;
assert (eval [ a; a; a; ab; a ] (U (True, And (AP "b", X (AP "a")))));;
assert (eval [ ab; a; b ] (U (AP "a", X (AP "a"))));;
print_endline "[OK] eval"

let f = Or (AP "a", Not (AP "b"));;

assert (
  PowerAPSet.equal (labels_of_formula ab f)
    (PowerAPSet.of_list [ ab; a; empty ]))

let f = And (AP "a", Not (AP "b"));;

assert (PowerAPSet.equal (labels_of_formula ab f) (PowerAPSet.of_list [ a ]))

let f = Not (AP "b");;

assert (
  PowerAPSet.equal (labels_of_formula ab f) (PowerAPSet.of_list [ empty; a ]))

let f = U (AP "a", And (Not (AP "a"), AP "b"));;

print_endline "[OK] labels_of_formula";;
assert (FormulaSet.cardinal (closure f) = 8);;

assert (
  FormulaSet.equal (closure f)
    (FormulaSet.of_list
       [
         AP "a";
         Not (AP "a");
         AP "b";
         Not (AP "b");
         And (Not (AP "a"), AP "b");
         Not (And (Not (AP "a"), AP "b"));
         f;
         Not f;
       ]))

let f = X (AP "a");;

assert (
  FormulaSet.equal (closure f)
    (FormulaSet.of_list [ AP "a"; Not (AP "a"); f; Not f ]))

let f = U (AP "t1", Not (X (AP "u1")));;

assert (
  FormulaSet.equal (closure f)
    (FormulaSet.of_list
       [
         AP "t1";
         Not (AP "t1");
         AP "u1";
         Not (AP "u1");
         X (AP "u1");
         Not (X (AP "u1"));
         f;
         Not f;
       ]))
;;

print_endline "[OK] closure"

let f = U (AP "a", And (Not (AP "a"), AP "b"))
let cl = closure f
let pcl = power_formulaset cl;;

assert (PowerFormulaSet.cardinal pcl = 256)

let bs = elementary_sets f;;

assert (PowerFormulaSet.cardinal bs = 6)

(* Elementary sets from Example 5.36 p. 277 Baier *)
let b1 =
  FormulaSet.of_list [ AP "a"; AP "b"; Not (And (Not (AP "a"), AP "b")); f ]

let b2 =
  FormulaSet.of_list [ AP "a"; AP "b"; Not (And (Not (AP "a"), AP "b")); Not f ]

let b3 =
  FormulaSet.of_list
    [ AP "a"; Not (AP "b"); Not (And (Not (AP "a"), AP "b")); f ]

let b4 =
  FormulaSet.of_list
    [ AP "a"; Not (AP "b"); Not (And (Not (AP "a"), AP "b")); Not f ]

let b5 =
  FormulaSet.of_list
    [ Not (AP "a"); Not (AP "b"); Not (And (Not (AP "a"), AP "b")); Not f ]

let b6 =
  FormulaSet.of_list [ Not (AP "a"); AP "b"; And (Not (AP "a"), AP "b"); f ]
;;

assert (
  PowerFormulaSet.equal bs (PowerFormulaSet.of_list [ b1; b2; b3; b4; b5; b6 ]))
;;

print_endline "[OK] elementary_sets"

(* --- *)

open Examples.Live_gnba
open Examples.Live_gnba.CritGNBA

let g' = to_nba g;;

assert (
  List.map StateSet.elements (PowerStateSet.elements g.fin)
  = [ [ "q1" ]; [ "q2" ] ])

let n =
  CritGNBA.StateSet.cardinal g.states * CritGNBA.PowerStateSet.cardinal g.fin
;;

assert (n = 6);;
assert (NumberedNba.StateSet.cardinal g'.states = n);;

assert (
  NumberedNba.StateSet.equal g'.init
    (NumberedNba.StateSet.of_list [ ("q0", 1) ]))
;;

assert (
  NumberedNba.StateSet.equal g'.fin (NumberedNba.StateSet.of_list [ ("q1", 1) ]))
;;

assert (
  NumberedNba.StateSet.equal
    (g'.func ("q2", 2) True)
    (NumberedNba.StateSet.of_list [ ("q0", 1) ]))
;;

assert (
  NumberedNba.StateSet.equal
    (g'.func ("q1", 1) True)
    (NumberedNba.StateSet.of_list [ ("q0", 2) ]))
;;

assert (
  NumberedNba.StateSet.equal
    (g'.func ("q0", 1) (AP Crit1))
    (NumberedNba.StateSet.of_list [ ("q1", 1) ]))
;;

assert (
  NumberedNba.StateSet.equal
    (g'.func ("q0", 1) True)
    (NumberedNba.StateSet.of_list [ ("q0", 1) ]))
;;

assert (
  NumberedNba.StateSet.equal
    (g'.func ("q0", 2) True)
    (NumberedNba.StateSet.of_list [ ("q0", 2) ]))
;;

print_endline "[OK] to_nba"

let f = X (AP "a")
let ap = APSet.of_list [ "a"; "b" ]
let g = gnba_of_formula ap f
let b1 = FormulaSet.of_list [ AP "a"; f ]
let b2 = FormulaSet.of_list [ AP "a"; Not f ]
let b3 = FormulaSet.of_list [ Not (AP "a"); f ]
let b4 = FormulaSet.of_list [ Not (AP "a"); Not f ];;

assert (
  PowerFormulaSet.equal g.states (PowerFormulaSet.of_list [ b1; b2; b3; b4 ]))
;;

assert (PowerFormulaSet.equal g.init (PowerFormulaSet.of_list [ b1; b3 ]));;
assert (FormulaGNBA.PowerStateSet.equal g.fin FormulaGNBA.PowerStateSet.empty);;
assert (PowerFormulaSet.equal (g.func b1 a) (PowerFormulaSet.of_list [ b1; b2 ]))
;;
assert (PowerFormulaSet.equal (g.func b1 ab) (g.func b1 a));;

(* "{a,b}" counts as "a" *)
assert (PowerFormulaSet.equal (g.func b1 empty) PowerFormulaSet.empty);;
assert (PowerFormulaSet.equal (g.func b2 a) (PowerFormulaSet.of_list [ b3; b4 ]))
;;
assert (PowerFormulaSet.equal (g.func b2 b) PowerFormulaSet.empty);;

assert (
  PowerFormulaSet.equal (g.func b3 empty) (PowerFormulaSet.of_list [ b1; b2 ]))
;;

assert (PowerFormulaSet.equal (g.func b3 b) (g.func b3 empty));;

(* "{b}" counts as "not a" *)
assert (
  PowerFormulaSet.equal (g.func b4 empty) (PowerFormulaSet.of_list [ b3; b4 ]))
;;

assert (PowerFormulaSet.equal (g.func b4 a) PowerFormulaSet.empty);;
assert (PowerFormulaSet.equal (g.func b4 a) (g.func b4 ab))

let f = U (AP "a", AP "b")
let ap = APSet.of_list [ "a"; "b" ]
let g = gnba_of_formula ap f
let b1 = FormulaSet.of_list [ AP "a"; AP "b"; f ]
let b2 = FormulaSet.of_list [ Not (AP "a"); AP "b"; f ]
let b3 = FormulaSet.of_list [ AP "a"; Not (AP "b"); f ]
let b4 = FormulaSet.of_list [ Not (AP "a"); Not (AP "b"); Not f ]
let b5 = FormulaSet.of_list [ AP "a"; Not (AP "b"); Not f ];;

assert (
  PowerFormulaSet.equal g.states
    (PowerFormulaSet.of_list [ b1; b2; b3; b4; b5 ]))
;;

assert (PowerFormulaSet.equal g.init (PowerFormulaSet.of_list [ b1; b2; b3 ]));;

assert (
  FormulaGNBA.PowerStateSet.equal g.fin
    (FormulaGNBA.PowerStateSet.singleton
       (PowerFormulaSet.of_list [ b1; b2; b4; b5 ])))
;;

assert (PowerFormulaSet.equal (g.func b1 ab) g.states);;
assert (PowerFormulaSet.equal (g.func b2 b) g.states);;
assert (PowerFormulaSet.equal (g.func b2 a) PowerFormulaSet.empty);;

assert (
  PowerFormulaSet.equal (g.func b3 a) (PowerFormulaSet.of_list [ b1; b2; b3 ]))
;;

assert (PowerFormulaSet.equal (g.func b4 empty) g.states);;
assert (PowerFormulaSet.equal (g.func b4 ab) PowerFormulaSet.empty);;
assert (PowerFormulaSet.equal (g.func b5 a) (PowerFormulaSet.of_list [ b4; b5 ]))
;;
print_endline "[OK] gnba_of_formula"

let f = U (AP "a", Not (X (AP "b")))
let ap = APSet.of_list [ "a"; "b" ]
let g = gnba_of_formula ap f
let b = nba_of_formula ap f;;

assert (FormulaGNBA.PowerStateSet.cardinal g.fin = 1);;

assert (
  FormulaGNBA.NumberedNba.StateSet.cardinal b.states
  = FormulaGNBA.StateSet.cardinal g.states)
;;

assert (
  FormulaGNBA.NumberedNba.StateSet.cardinal b.init
  = FormulaGNBA.StateSet.cardinal g.init)
;;

assert (
  FormulaGNBA.NumberedNba.StateSet.cardinal b.fin
  = FormulaGNBA.StateSet.cardinal
      (List.nth (FormulaGNBA.PowerStateSet.elements g.fin) 0))
;;

print_endline "[OK] nba_of_formula"

let f = U (AP "a", Not (X (AP "b")))
let ap = APSet.of_list [ "a"; "b" ]
let n = petrinet_of_formula ap f;;

assert (FormulaPTNet.is_statemachine n)

open String_product
open StringPTNetProduct
module StringFullsync = Fullsync.MakeEH (StringPTNetProduct)
open StringFullsync
open TesterLtl

let f = X (AP "u1")
let mg = get_marking_graph Prod7.prod7 ()
let m1 = PlaceSet.of_list [ "t1"; "u1" ]
let m2 = PlaceSet.of_list [ "t2"; "u1" ]
let m3 = PlaceSet.of_list [ "t1"; "u2" ]
let m4 = PlaceSet.of_list [ "t2"; "u2" ]
let e = APSet.empty
let ru1 = APSet.of_list [ "u1" ]
let rt1 = APSet.of_list [ "t1" ]
let rt2 = APSet.of_list [ "t2" ]
let a = [ `Idle; `T "a" ]
let b = [ `Idle; `T "b" ]
let c = [ `T "c"; `Idle];;

assert (
  APSet.equal (f_state_set m1 f) (apset_of_placeset (PlaceSet.of_list [ "u1" ])))
;;

assert (
  APSet.equal (f_state_set m2 f) (apset_of_placeset (PlaceSet.of_list [ "u1" ])))
;;

assert (APSet.equal (f_state_set m3 f) APSet.empty);;
assert (APSet.equal (f_state_set m4 f) APSet.empty);;
assert (is_f_step (ru1, c, ru1) mg f);;
assert (is_f_step (ru1, c, e) mg f = false);;
assert (is_f_step (e, c, ru1) mg f = false);;
assert (is_f_step (e, c, e) mg f);;
assert (is_f_step (ru1, a, ru1) mg f = false);;
assert (is_f_step (ru1, b, ru1) mg f = false);;
assert (is_f_step (ru1, b, e) mg f = false);;
assert (is_f_step (e, b, ru1) mg f);;
assert (is_f_step (ru1, a, e) mg f);;
assert (is_f_step (e, a, ru1) mg f = false);;
assert (is_f_step (e, a, e) mg f = false);;
assert (is_f_step (ru1, c, rt1) mg f = false);;
assert (is_f_step (rt1, c, rt1) mg f = false);;
assert (is_f_step (rt1, a, rt1) mg f = false);;

assert (is_f_occurrence_sequence [ c; c ] mg f);;
assert (is_occurrence_sequence [ c; c ] Prod7.prod7 = false);;

assert (is_f_occurrence_sequence [ a; a ] mg f = false);;

let f = X (AP "t1");;

assert (is_f_step (rt1, c, rt1) mg f = false);;
assert (is_f_step (rt1, c, rt2) mg f = false);;
assert (is_f_step (rt1, c, e) mg f);;
assert (is_f_step (e, c, rt1) mg f = false);;
assert (is_f_step (rt1, a, rt1) mg f);;
assert (is_f_step (e, a, e) mg f);;
assert (is_f_step (rt1, b, rt1) mg f);;

let f = U (AP "u1", AP "t2");;
assert (is_f_step (ru1, a, rt2) mg f = false);;
assert (is_f_step (ru1, b, rt2) mg f = false);;
assert (is_f_step (ru1, c, rt2) mg f = false);;

print_endline "[OK] phi steps and histories"

open String_ltl.StringNetfullsync;;

(* prod7 cannot reach these markings*)
assert (let r = test Prod7.prod7 (F (And (AP "u1", AP "u2"))) in r.res = false);;
assert (let r = test Prod7.prod7 (F (And (AP "t1", AP "t2"))) in r.res = false);;
assert (let r = test Prod7.prod7 (G (And (AP "u1", AP "u2"))) in r.res = false);;

assert (let r = test Prod7.prod7 (G (And (AP "u1", X (AP "u2")))) in r.res = false);;
assert (let r = test Prod7.prod7 (F (And (AP "u1", X (AP "u2")))) in r.res);;

assert (let r = test Prod7.prod7 (F (And (AP "t2", X (AP "t1")))) in r.res = false);; (* prod7 cannot visit t1 from t2 *)
assert (let r = test Prod7.prod7 (Not (F (And (AP "t2", X (AP "t1"))))) in r.res);; (* negation works *)

assert (let r = test Prod7.prod7 (F (And (AP "t2", F (AP "t1")))) in r.res = false);; (* prod7 cannot visit t1 from t2 ever again *)
assert (let r = test Prod7.prod7 (F (And (AP "u2", F (AP "u1")))) in r.res);;

assert (let r = test Prod7.prod7 (G (F (AP "u1"))) in r.res);; (* prod7 can visit u1 infinitely often *)
assert (let r = test Prod7.prod7 (F (G (AP "u1"))) in r.res = false);; (* prod7 cannot get stuck in u1 forever *)

assert (let r = test Prod7.prod7 (G (F (AP "u2"))) in r.res);; (* prod7 can visit u1 infinitely often *)
assert (let r = test Prod7.prod7 (F (G (AP "u2"))) in r.res = false);; (* prod7 cannot get stuck in u2 forever *)

assert (let r = test Prod7.prod7 (G (F (AP "u1"))) in r.res);; (* prod7 can visit t1 infinitely often *)
assert (let r = test Prod7.prod7 (F (G (AP "t1"))) in r.res = false);; (* prod7 CAN get stuck in t1 forever *)

assert (let r = test Prod7.prod7 (F (G (AP "t2"))) in r.res);; (* prod7 can indeed get stuck in t2 forever *)
assert (let r = test Prod7.prod7 (G (AP "t2")) in r.res = false);; (* prod7 doesn't start from t2 *)
assert (let r = test Prod7.prod7 (G (G (AP "t2"))) in r.res = false);; (* this is equal to G (AP t2) *)
assert (let r = test Prod7.prod7 (F (And (AP "s4", AP "r3"))) in r.res = false);; (* s4 is not a place of Prod7.prod7 *)
assert (let r = test Prod7.prod7 (F (Or (AP "s4", AP "u1"))) in r.res);;

assert (let r = test Prod2.prod2 (And (AP "s4", AP "s3")) in r.res = false);; (* Prod2.prod2 doesn't start from this marking *)
assert (let r = test Prod2.prod2 (F (And (AP "s4", AP "r3"))) in r.res);; (* Prod2.prod2 will eventually reach this marking *)
assert (let r = test Prod2.prod2 (F (And (AP "s4", AP "r1"))) in r.res);; (* also this marking *)
assert (let r = test ~stutter:true Prod2.prod2 (F (And (AP "s2", AP "s3"))) in r.res = false);; (* but not this one - conflict *)
assert (let r = test ~stutter:true Prod2.prod2 (F (And (AP "s1", AP "s2"))) in r.res = false);; (* neither this one - causality *)
assert (let r = test ~stutter:true Prod2.prod2 (F (And (AP "s1", F (AP "s2")))) in r.res);; (* should be true *)

(* using ~stutter:true makes unfolding faster, but it's risky when f is not X-free *)
assert (let r = test ~stutter:true Prod2.prod2 (F (And (AP "s2", X (AP "s4")))) in r.res);; (* having a token in s2 is not sufficient to move to s4 *)
assert (let r = test ~stutter:true Prod2.prod2 (F (And (And (AP "s2", AP "r2"), X (AP "s4")))) in r.res);; (* there are sufficient conditions to move to s4 *)

(* ##### FAILURES ##### *)
assert (let r = test Prod7.prod7 (G (F (AP "t2"))) in r.res = false);; (* fails *)
assert (let r = test ~stutter:true Prod2.prod2 (F (G (AP "s4"))) in r.res = false);; (* fails - this doesn't make sense *)