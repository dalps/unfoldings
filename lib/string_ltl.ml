module StringLtl = Ltl.Make (String)
open StringLtl
open StringLtl.Formula

let rec string_of_formula = function
  | True -> "true"
  | False -> "false"
  | AP a -> a
  | Not f -> "¬" ^ string_of_formula f
  | X f -> "X " ^ string_of_formula f
  | Or (f1, f2) -> string_of_formula f1 ^ " V " ^ string_of_formula f2
  | And (f1, f2) -> string_of_formula f1 ^ " ∧ " ^ string_of_formula f2
  | U (f1, f2) -> string_of_formula f1 ^ " U " ^ string_of_formula f2

let string_of_formulaset fset =
  if FormulaSet.is_empty fset then "∅"
  else
    "\"{"
    ^ String.concat "; "
        (FormulaSet.fold (fun f -> List.cons (string_of_formula f)) fset [])
    ^ "}\""

let string_of_apset apset =
  if APSet.is_empty apset then "∅"
  else
    "\"{"
    ^ String.concat "; " (APSet.fold (fun ap -> List.cons ap) apset [])
    ^ "}\""

let string_of_node =
  FormulaPTNet.Node.(
    function
    | P (p, _) -> string_of_formulaset p | T (_, t) -> string_of_apset t)
