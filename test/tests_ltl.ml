open Ltllib
open Modelchecklib
open Stringnetlib
open Examples
module StringLtl = Ltl.Make (String)
open Ltl
open StringLtl

let%test "" = (length (X (AP "a")) = 1)
let%test "" = (length (And (X (AP "a"), AP "b")) = 2)
let%test "" = (length (U (X (AP "a"), AP "b")) = 2)
let%test "" = (length (U (X (AP "a"), And (AP "a", AP "b"))) = 3)
let%test "" = (length (U (X (AP "a"), And (AP "a", Not (AP "b")))) = 4)
(* --- end length --- *)

let ab = APSet.of_list [ "a"; "b" ]
let a = APSet.of_list [ "a" ]
let b = APSet.of_list [ "b" ]
let empty = APSet.empty

let%test "" = (eval [ ab; a; a ] (And (AP "a", AP "b")))
let%test "" = (eval [ ab; a; a ] (And (AP "a", X (AP "b"))) = false)
let%test "" = (eval [ ab; a; a ] (And (AP "a", X (AP "a"))))
let%test "" = (eval [ ab; empty; b ] (X (AP "a")) = false)
let%test "" = (eval [ ab; a; a ] (U (AP "a", X (AP "a"))))
let%test "" = (eval [ ab; a; a ] (And (AP "a", Not (X (AP "a")))) = false)
let%test "" = (eval [ ab; a; a; b ] (U (AP "a", AP "b")))
let%test "" = (eval [ ab; a; a; b ] (U (AP "a", And (AP "b", AP "a"))))
let%test "" = (eval [ ab ] (U (AP "a", And (AP "b", AP "a"))))
let%test "" = (eval [ a; a; a; ab ] (U (AP "a", And (AP "b", AP "a"))))
let%test "" = (eval [ a; a; a; b ] (U (AP "a", And (AP "b", AP "a"))) = false)
let%test "" = (eval [ a; ab; a; ab ] (U (AP "a", And (AP "b", AP "a"))))
let%test "" = (eval [ a; b; a; ab ] (U (AP "a", And (AP "b", AP "a"))) = false)
let%test "" = (eval [ empty; ab; a; ab ] (U (AP "a", And (AP "b", AP "a"))) = false)
let%test "" = (eval [ a; a; a; ab; a ] (U (True, And (AP "b", AP "a"))))
let%test "" = (eval [ a; a; a; ab ] (U (True, And (AP "b", X (AP "a")))) = false)
let%test "" = (eval [ a; a; a; ab; a ] (U (True, And (AP "b", X (AP "a")))))
let%test "" = (eval [ ab; a; b ] (U (AP "a", X (AP "a"))))
(* --- eval --- *)

let f = Or (AP "a", Not (AP "b"))

let%test "" = (
  PowerAPSet.equal (labels_of_formula ab f)
    (PowerAPSet.of_list [ ab; a; empty ]))

let f = And (AP "a", Not (AP "b"))

let%test "" = (PowerAPSet.equal (labels_of_formula ab f) (PowerAPSet.of_list [ a ]))

let f = Not (AP "b")

let%test "" = (
  PowerAPSet.equal (labels_of_formula ab f) (PowerAPSet.of_list [ empty; a ]))

let f = U (AP "a", And (Not (AP "a"), AP "b"))

(* --- end labels_of_formula --- *)
let%test "" = (FormulaSet.cardinal (closure f) = 8)

let%test "" = (
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

let f = X (AP "a")

let%test "" = (
  FormulaSet.equal (closure f)
    (FormulaSet.of_list [ AP "a"; Not (AP "a"); f; Not f ]))

let f = U (AP "t1", Not (X (AP "u1")))

let%test "" = (
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


(* --- closure --- *)

let f = U (AP "a", And (Not (AP "a"), AP "b"))
let cl = closure f
let pcl = power_formulaset cl

let%test "" = (PowerFormulaSet.cardinal pcl = 256)

let bs = elementary_sets f

let%test "" = (PowerFormulaSet.cardinal bs = 6)

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


let%test "" = (
  PowerFormulaSet.equal bs (PowerFormulaSet.of_list [ b1; b2; b3; b4; b5; b6 ]))


(* --- end elementary_sets --- *)

open Examples.Live_gnba
open Examples.Live_gnba.CritGNBA

let g' = to_nba g

let%test "" = (
  List.map StateSet.elements (PowerStateSet.elements g.fin)
  = [ [ "q1" ]; [ "q2" ] ])

let n =
  CritGNBA.StateSet.cardinal g.states * CritGNBA.PowerStateSet.cardinal g.fin


let%test "" = (n = 6)
let%test "" = (NumberedNba.StateSet.cardinal g'.states = n)

let%test "" = (
  NumberedNba.StateSet.equal g'.init
    (NumberedNba.StateSet.of_list [ ("q0", 1) ]))


let%test "" = (
  NumberedNba.StateSet.equal g'.fin (NumberedNba.StateSet.of_list [ ("q1", 1) ]))


let%test "" = (
  NumberedNba.StateSet.equal
    (g'.func ("q2", 2) True)
    (NumberedNba.StateSet.of_list [ ("q0", 1) ]))


let%test "" = (
  NumberedNba.StateSet.equal
    (g'.func ("q1", 1) True)
    (NumberedNba.StateSet.of_list [ ("q0", 2) ]))


let%test "" = (
  NumberedNba.StateSet.equal
    (g'.func ("q0", 1) (AP Crit1))
    (NumberedNba.StateSet.of_list [ ("q1", 1) ]))


let%test "" = (
  NumberedNba.StateSet.equal
    (g'.func ("q0", 1) True)
    (NumberedNba.StateSet.of_list [ ("q0", 1) ]))


let%test "" = (
  NumberedNba.StateSet.equal
    (g'.func ("q0", 2) True)
    (NumberedNba.StateSet.of_list [ ("q0", 2) ]))


(* --- end to_nba --- *)

let f = X (AP "a")
let ap = APSet.of_list [ "a"; "b" ]
let g = gnba_of_formula ap f
let b1 = FormulaSet.of_list [ AP "a"; f ]
let b2 = FormulaSet.of_list [ AP "a"; Not f ]
let b3 = FormulaSet.of_list [ Not (AP "a"); f ]
let b4 = FormulaSet.of_list [ Not (AP "a"); Not f ]

let%test "" = (
  PowerFormulaSet.equal g.states (PowerFormulaSet.of_list [ b1; b2; b3; b4 ]))


let%test "" = (PowerFormulaSet.equal g.init (PowerFormulaSet.of_list [ b1; b3 ]))
let%test "" = (FormulaGNBA.PowerStateSet.equal g.fin FormulaGNBA.PowerStateSet.empty)
let%test "" = (PowerFormulaSet.equal (g.func b1 a) (PowerFormulaSet.of_list [ b1; b2 ]))

let%test "" = (PowerFormulaSet.equal (g.func b1 ab) (g.func b1 a))

(* "{a,b}" counts as "a" *)
let%test "" = (PowerFormulaSet.equal (g.func b1 empty) PowerFormulaSet.empty)
let%test "" = (PowerFormulaSet.equal (g.func b2 a) (PowerFormulaSet.of_list [ b3; b4 ]))

let%test "" = (PowerFormulaSet.equal (g.func b2 b) PowerFormulaSet.empty)

let%test "" = (
  PowerFormulaSet.equal (g.func b3 empty) (PowerFormulaSet.of_list [ b1; b2 ]))


let%test "" = (PowerFormulaSet.equal (g.func b3 b) (g.func b3 empty))

(* "{b}" counts as "not a" *)
let%test "" = (
  PowerFormulaSet.equal (g.func b4 empty) (PowerFormulaSet.of_list [ b3; b4 ]))


let%test "" = (PowerFormulaSet.equal (g.func b4 a) PowerFormulaSet.empty)
let%test "" = (PowerFormulaSet.equal (g.func b4 a) (g.func b4 ab))

let f = U (AP "a", AP "b")
let ap = APSet.of_list [ "a"; "b" ]
let g = gnba_of_formula ap f
let b1 = FormulaSet.of_list [ AP "a"; AP "b"; f ]
let b2 = FormulaSet.of_list [ Not (AP "a"); AP "b"; f ]
let b3 = FormulaSet.of_list [ AP "a"; Not (AP "b"); f ]
let b4 = FormulaSet.of_list [ Not (AP "a"); Not (AP "b"); Not f ]
let b5 = FormulaSet.of_list [ AP "a"; Not (AP "b"); Not f ]

let%test "" = (
  PowerFormulaSet.equal g.states
    (PowerFormulaSet.of_list [ b1; b2; b3; b4; b5 ]))


let%test "" = (PowerFormulaSet.equal g.init (PowerFormulaSet.of_list [ b1; b2; b3 ]))

let%test "" = (
  FormulaGNBA.PowerStateSet.equal g.fin
    (FormulaGNBA.PowerStateSet.singleton
       (PowerFormulaSet.of_list [ b1; b2; b4; b5 ])))


let%test "" = (PowerFormulaSet.equal (g.func b1 ab) g.states)
let%test "" = (PowerFormulaSet.equal (g.func b2 b) g.states)
let%test "" = (PowerFormulaSet.equal (g.func b2 a) PowerFormulaSet.empty)

let%test "" = (
  PowerFormulaSet.equal (g.func b3 a) (PowerFormulaSet.of_list [ b1; b2; b3 ]))


let%test "" = (PowerFormulaSet.equal (g.func b4 empty) g.states)
let%test "" = (PowerFormulaSet.equal (g.func b4 ab) PowerFormulaSet.empty)
let%test "" = (PowerFormulaSet.equal (g.func b5 a) (PowerFormulaSet.of_list [ b4; b5 ]))

(* --- end gnba_of_formula --- *)

let f = U (AP "a", Not (X (AP "b")))
let ap = APSet.of_list [ "a"; "b" ]
let g = gnba_of_formula ap f
let b = nba_of_formula ap f

let%test "" = (FormulaGNBA.PowerStateSet.cardinal g.fin = 1)

let%test "" = (
  FormulaGNBA.NumberedNba.StateSet.cardinal b.states
  = FormulaGNBA.StateSet.cardinal g.states)


let%test "" = (
  FormulaGNBA.NumberedNba.StateSet.cardinal b.init
  = FormulaGNBA.StateSet.cardinal g.init)


let%test "" = (
  FormulaGNBA.NumberedNba.StateSet.cardinal b.fin
  = FormulaGNBA.StateSet.cardinal
      (List.nth (FormulaGNBA.PowerStateSet.elements g.fin) 0))


(* --- end nba_of_formula --- *)

let f = U (AP "a", Not (X (AP "b")))
let ap = APSet.of_list [ "a"; "b" ]
let n = petrinet_of_formula ap f

let%test "" = (FormulaPTNet.is_statemachine n)

open String_product
open StringPTNetProduct
module StringFullsync = Stuttering.Make (StringPTNetProduct)
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
let c = [ `T "c"; `Idle]

let%test "" = (
  APSet.equal (f_state_set m1 f) (apset_of_placeset (PlaceSet.of_list [ "u1" ])))


let%test "" = (
  APSet.equal (f_state_set m2 f) (apset_of_placeset (PlaceSet.of_list [ "u1" ])))


let%test "" = (APSet.equal (f_state_set m3 f) APSet.empty)
let%test "" = (APSet.equal (f_state_set m4 f) APSet.empty)
let%test "" = (is_f_step (ru1, c, ru1) mg f)
let%test "" = (is_f_step (ru1, c, e) mg f = false)
let%test "" = (is_f_step (e, c, ru1) mg f = false)
let%test "" = (is_f_step (e, c, e) mg f)
let%test "" = (is_f_step (ru1, a, ru1) mg f = false)
let%test "" = (is_f_step (ru1, b, ru1) mg f = false)
let%test "" = (is_f_step (ru1, b, e) mg f = false)
let%test "" = (is_f_step (e, b, ru1) mg f)
let%test "" = (is_f_step (ru1, a, e) mg f)
let%test "" = (is_f_step (e, a, ru1) mg f = false)
let%test "" = (is_f_step (e, a, e) mg f = false)
let%test "" = (is_f_step (ru1, c, rt1) mg f = false)
let%test "" = (is_f_step (rt1, c, rt1) mg f = false)
let%test "" = (is_f_step (rt1, a, rt1) mg f = false)

let%test "" = (is_f_occurrence_sequence [ c; c ] mg f)
let%test "" = (is_occurrence_sequence [ c; c ] Prod7.prod7 = false)

let%test "" = (is_f_occurrence_sequence [ a; a ] mg f = false)

let f = X (AP "t1")

let%test "" = (is_f_step (rt1, c, rt1) mg f = false)
let%test "" = (is_f_step (rt1, c, rt2) mg f = false)
let%test "" = (is_f_step (rt1, c, e) mg f)
let%test "" = (is_f_step (e, c, rt1) mg f = false)
let%test "" = (is_f_step (rt1, a, rt1) mg f)
let%test "" = (is_f_step (e, a, e) mg f)
let%test "" = (is_f_step (rt1, b, rt1) mg f)

let f = U (AP "u1", AP "t2")
let%test "" = (is_f_step (ru1, a, rt2) mg f = false)
let%test "" = (is_f_step (ru1, b, rt2) mg f = false)
let%test "" = (is_f_step (ru1, c, rt2) mg f = false)

(* --- end phi steps and histories --- *)

open String_ltl.StringNetfullsync
open Result
(* using ~stutter:true makes unfolding faster, but it's risky when f is not X-free *)

let test_product prod = List.for_all 
  (fun (outcome, ltl) -> 
    let select o = if o then Result.is_ok else Result.is_error in
      select outcome @@ test prod ltl
      (* && select (not outcome) @@ test prod (Not ltl) *)
  )

(* outcome, test *)
let prod7_tests = [
  false, F (And (AP "t1", AP "t2")); (* prod7 can never be in both t1 and t2 at the same time *)
  false, F (And (AP "u1", AP "u2")); (* prod7 can never be in both u2 and u1 at the same time *)
  false, F (And (AP "u2", AP "u1")); (* and commutativity *)
  false, G (And (AP "u1", AP "u2")); (* same as before *)

  true, F (Or (AP "u2", AP "u1")); (* prod7 will eventually be in either u2 or u1 *)
  true, G (Or (AP "u2", AP "u1")); (* prod7 is always in either u2 or u1 *)

  true, F (And (AP "u1", X (AP "u2"))); (* all infinite runs execute <ϵ,a> at least once *)
  false, G (And (AP "u1", X (AP "u2"))); (* violated by all infinite runs as it requires prod7 to stay in u1 indefinitely *)
  true, F (If (AP "u1", X (AP "u2"))); (* more sensible *)
  true, F (Or (Not (AP "u1"), X (AP "u2"))); (* if equivalence *)
  true, F (G (If (AP "u1", X (AP "u2")))); (* all infinite runs alternate between u1 and u2 forever after (if ever) executing <c,ϵ> *)
    
  true, F (AP "t1"); (* obvious *)
  false, F (AP "t2"); (* violated by the infinite run that never executes <c,ϵ> *)

  false, F (And (AP "t2", X (AP "t1"))); (* prod7 cannot visit t1 from t2 *)
  true, Not (F (And (AP "t2", X (AP "t1")))); (* negation works *)

  false, F (And (AP "t2", F (AP "t1"))); (* prod7 cannot visit t1 from t2 ever again *)
  true, F (And (AP "u2", X (AP "u1"))); (* prod7 can visit u2 from u1 *)
  true, F (And (AP "u2", F (AP "u1"))); (* prod7 can visit u2 from u1 *)

  true, G (F (AP "u1")); (* prod7 can visit u1 infinitely often *)
  false, F (G (AP "u1")); (* prod7 cannot get stuck in u1 forever *)

  true, G (F (AP "u2")); (* prod7 can visit u1 infinitely often *)
  false, F (G (AP "u2")); (* prod7 cannot get stuck in u2 forever *)

  true, G (F (AP "u1")); (* prod7 can visit t1 infinitely often *)
  false, F (G (AP "t1")); (* prod7 CAN get stuck in t1 forever *)

  false, G (F (AP "t2")); (* t2 can be visited only once *)
  false, F (G (AP "t2")); (* prod7 doesn't get stuck forever in t2 in all runs *)
  false, G (AP "t2"); (* prod7 doesn't start from t2 *)
  false, G (G (AP "t2")); (* this is equal to 'G (AP "t2")' *)

  false, F (And (AP "s4", AP "r3")); (* s4 is not a place of Prod7.prod7 *)
  true, F (Or (AP "s4", AP "u1")); (* s4 is not a place of Prod7.prod7, but u1 is *)
]

let%test "prod7-unreachable-markings" = test_product Prod7.prod7 prod7_tests

let%test "" = (is_error (test Prod2.prod2 (And (AP "s4", AP "s3")))) (* Prod2.prod2 doesn't start from this marking *)
let%test "" = (is_ok (test Prod2.prod2 (F (And (AP "s4", AP "r3"))))) (* Prod2.prod2 will eventually reach this marking *)
let%test "" = (is_error (test Prod2.prod2 (F (And (AP "s4", AP "r1"))))) (* violated by the run that always executes <t5,ϵ> from the marking {s4,r3} *)
let%test "" = (is_error (test Prod2.prod2 (F (And (AP "s2", AP "s3"))))) (* but not this one - conflict *)
let%test "" = (is_error (test Prod2.prod2 (F (And (AP "s1", AP "s2"))))) (* neither this one - causality *)
let%test "" = (is_error (test Prod2.prod2 (F (And (AP "s1", F (AP "s2")))))) (* violated by the run that always takes the s3 route *)
let%test "" = (is_error (test Prod2.prod2 (F (G (AP "s4"))))) (* no run stays forever in s4 *)

let%test "" = (is_error (test Prod2.prod2 (F (And (AP "s2", X (AP "s4")))))) (* violated by the infinite run that always passes through s3 to go to s4 and never visit s2 *)
let%test "" = (is_error (test Prod2.prod2 (F (And (And (AP "s2", AP "r2"), X (AP "s4")))))) (* same *)

let%test "" = (is_error (test Prod2.prod2 (F (AP "s3")))) (* violated by the infinite run that always passes through s2 to go to s4 and never visit s3 *)
let%test "" = (is_error (test Prod2.prod2 (F (AP "s2")))) (* violated by the infinite run that always passes through s3 to go to s4 and never visit s2 *)

let%test "" = (is_ok (test ~stutter:true Prod2.prod2 (G (F (AP "r1")))))
let%test "" = (is_ok (test ~stutter:true Prod2.prod2 (G (F (AP "r2")))))
let%test "" = (is_ok (test ~stutter:true Prod2.prod2 (G (F (AP "r3")))))
let%test "" = (is_error (test Prod2.prod2 (G (F (AP "s2")))))
let%test "" = (is_error (test Prod2.prod2 (G (F (AP "s3")))))

let%test "" = (is_error (test Prod7.prod7 (G (If (AP "u1", X (AP "u1")))))) (* violated by all runs *)
let%test "" = (is_error (test Prod7.prod7 (G (Or (Not (AP "u1"), X (AP "u1")))))) (* violated by runs that execute <c,ϵ> *)

(* ##### FALSE POSITIVES ##### *)
(* these two assume <c,ϵ> will be executed eventually *)
let%test "" = (is_ok (test Prod2.prod2 (F (And (AP "s2", F (AP "s4")))))) (* ??? should be violated by the infinite run that always passes through s3 to go to s4 and never visit s2 *)
let%test "" = (is_ok (test Prod7.prod7 (G (If (AP "u1", X (AP "u2")))))) (* ??? should be violated by runs that execute <c,ϵ> *)
let%test "" = (is_ok (test Prod7.prod7 (G (If (AP "t1", X (AP "t2")))))) (* ??? should be violated by runs that never execute <c,ϵ> *)
