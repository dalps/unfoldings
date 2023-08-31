print_endline "#### Running ltl tests..."

module StringLtl = Unfoldings.Ltl.Make (String)
open StringLtl
open StringLtl.Formula;;

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
assert (PowerPowerFormulaSet.equal g.fin PowerPowerFormulaSet.empty);;
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
  PowerPowerFormulaSet.equal g.fin
    (PowerPowerFormulaSet.singleton
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
