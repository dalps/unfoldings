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
;;

print_endline "[OK] closure"

let f = U (AP "a", And (Not (AP "a"), AP "b"))
let cl = closure f
let pcl = powerformulaset cl;;

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
