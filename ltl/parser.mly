%parameter<Semantics : Parser_sig.S>

%{
  open Semantics
%}

%start <LTL.Formula.t> main

%left "=>" "<=>"
%left "||"
%left "&&"
%left "U"
%nonassoc "!" "X" "F" "G"

%%

let main := ~ = expression; EOF; <>

let expression :=
| "("; ~ = expression; ")"; <>
| "true"; { True }
| "false"; { False }
| ~ = atomic_proposition; <AP>
| "!"; ~ = expression; <Not>
| "X"; ~ = expression; <X>
| "F"; ~ = expression; <F>
| "G"; ~ = expression; <G>
| e1 = expression; "U"; e2 = expression; <U>
| e1 = expression; "||"; e2 = expression; <Or>
| e1 = expression; "&&"; e2 = expression; <And>
| e1 = expression; "=>"; e2 = expression; <If>
| e1 = expression; "<=>"; e2 = expression; <Iff>

let atomic_proposition := id = "x"; { inject id }
