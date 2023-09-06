open Ltl
open Setprintr
open String_product
module StringLtl = Ltl.Make (String)
module StringFullsync = Fullsync.Make (StringPTNetProduct)
module StringNetfullsync = Fullsync.MakeEH (StringPTNetProduct)

module UnfoldTester =
  Fullsync.MakeEH (StringProductUnfolder.Unfolder.OccurrenceNet)

let string_of_stringformula = string_of_formula Fun.id
let string_of_transformula = string_of_formula string_of_globaltrans
let string_of_tokenformula = string_of_formula string_of_token

let string_of_formulaset =
  string_of_set (module StringLtl.FormulaSet) string_of_stringformula

let string_of_syncformulaset =
  string_of_set
    (module StringFullsync.TesterLtl.FormulaSet)
    string_of_transformula

let string_of_netformulaset =
  string_of_set
    (module StringNetfullsync.TesterLtl.FormulaSet)
    string_of_stringformula

let string_of_tokenformulaset =
  string_of_set
    (module UnfoldTester.TesterLtl.FormulaSet)
    string_of_tokenformula

let string_of_apset = string_of_set (module StringLtl.APSet) Fun.id

let label_of_node =
  StringLtl.FormulaPTNet.Node.(
    function
    | P (p, i) -> string_of_formulaset p ^ string_of_int i
    | T (i, t) -> string_of_int i ^ string_of_apset t)

let string_of_node =
  StringLtl.FormulaPTNet.Node.(
    function
    | P (p, _) -> string_of_formulaset p | T (_, t) -> string_of_apset t)

let name_of_numberedstate (fset, k) =
  string_of_formulaset fset ^ "\nCopy " ^ string_of_int k

let name_of_syncnumberedstate (fset, k) =
  string_of_syncformulaset fset ^ "#" ^ string_of_int k

let name_of_sync_node (s, q) =
  "(" ^ string_of_placeset s ^ "," ^ name_of_syncnumberedstate q ^ ")"

let string_of_netsyncplace = function
  | `NetP p -> p
  | `NbaP (b, i) -> string_of_netformulaset b ^ "#" ^ string_of_int i

let string_of_netsynctrans =
  let open StringNetfullsync.NetGNBA.NumberedNba in
  function
  | t, u ->
      "[" ^ string_of_globaltrans t ^ ", "
      ^ (match u with `Idle -> "ϵ" | `U e -> "&beta;" ^ string_of_int e.id)
      ^ "]"

let string_of_netsyncnode =
  StringNetfullsync.SyncNet.Node.(
    function P p -> string_of_netsyncplace p | T t -> string_of_netsynctrans t)

let string_of_unfoldsyncplace = function
  | `NetP p -> name_of_token p
  | `NbaP (b, i) -> string_of_tokenformulaset b ^ "#" ^ string_of_int i

let string_of_unfoldsynctrans =
  let open UnfoldTester.NetGNBA.NumberedNba in
  function
  | t, u ->
      "[" ^ name_of_event t ^ ", "
      ^ (match u with `Idle -> "ϵ" | `U e -> "&beta;" ^ string_of_int e.id)
      ^ "]"

let string_of_unfoldsyncnode =
  UnfoldTester.SyncNet.Node.(
    function
    | P p -> string_of_unfoldsyncplace p | T t -> string_of_unfoldsynctrans t)
