module StringLtl = Ltl.Make (String)
module StringFullsync = Fullsync.Make (String_product.StringPTNetProduct)
open StringLtl
open StringLtl.Formula

let rec string_of_formula = function
  | True -> "true"
  | False -> "false"
  | AP a -> a
  | Not (AP _ as f) -> "¬" ^ string_of_formula f
  | Not f -> "¬(" ^ string_of_formula f ^ ")"
  | X f -> "X " ^ string_of_formula f
  | F f -> "F " ^ string_of_formula f
  | G f -> "G " ^ string_of_formula f
  | If (f1, f2) -> string_of_formula f1 ^ " => " ^ string_of_formula f2
  | Iff (f1, f2) -> string_of_formula f1 ^ " <=> " ^ string_of_formula f2
  | Or (f1, f2) -> string_of_formula f1 ^ " ∨ " ^ string_of_formula f2
  | And (f1, f2) -> string_of_formula f1 ^ " ∧ " ^ string_of_formula f2
  | U (f1, f2) -> string_of_formula f1 ^ " U " ^ string_of_formula f2

let string_of_formulaset fset =
  if FormulaSet.is_empty fset then "∅"
  else
    "{"
    ^ String.concat ", "
        (FormulaSet.fold (fun f -> List.cons (string_of_formula f)) fset [])
    ^ "}"

let rec string_of_syncformula =
  StringFullsync.TesterLtl.Formula.(
    function
    | True -> "true"
    | False -> "false"
    | AP a -> String_product.string_of_globaltrans a
    | Not (AP _ as f) -> "¬" ^ string_of_syncformula f
    | Not f -> "¬(" ^ string_of_syncformula f ^ ")"
    | X f -> "X " ^ string_of_syncformula f
    | F f -> "F " ^ string_of_syncformula f
    | G f -> "G " ^ string_of_syncformula f
    | If (f1, f2) ->
        string_of_syncformula f1 ^ " => " ^ string_of_syncformula f2
    | Iff (f1, f2) ->
        string_of_syncformula f1 ^ " <=> " ^ string_of_syncformula f2
    | Or (f1, f2) -> string_of_syncformula f1 ^ " ∨ " ^ string_of_syncformula f2
    | And (f1, f2) ->
        string_of_syncformula f1 ^ " ∧ " ^ string_of_syncformula f2
    | U (f1, f2) -> string_of_syncformula f1 ^ " U " ^ string_of_syncformula f2)

let string_of_syncformulaset fset =
  StringFullsync.TesterLtl.(
    if FormulaSet.is_empty fset then "∅"
    else
      "{"
      ^ String.concat ", "
          (FormulaSet.fold
             (fun f -> List.cons (string_of_syncformula f))
             fset [])
      ^ "}")

let string_of_apset apset =
  if APSet.is_empty apset then "∅"
  else
    "{"
    ^ String.concat ", " (APSet.fold (fun ap -> List.cons ap) apset [])
    ^ "}"

let label_of_node =
  FormulaPTNet.Node.(
    function
    | P (p, i) -> string_of_formulaset p ^ string_of_int i
    | T (i, t) -> string_of_int i ^ string_of_apset t)

let string_of_node =
  FormulaPTNet.Node.(
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
