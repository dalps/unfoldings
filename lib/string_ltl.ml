open Ltl
open Setprintr
module StringLtl = Ltl.Make (String)
module StringFullsync = Fullsync.Make (String_product.StringPTNetProduct)
module StringNetfullsync = Fullsync.MakeEH (String_product.StringPTNetProduct)

let string_of_stringformula = string_of_formula Fun.id

let string_of_transformula =
  string_of_formula String_product.string_of_globaltrans

let string_of_tokenformula =
  string_of_formula String_product.string_of_token

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
  "("
  ^ String_product.string_of_placeset s
  ^ ","
  ^ name_of_syncnumberedstate q
  ^ ")"
