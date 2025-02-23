open Stringnetlib
open String_product
open Examples;;

(* Plot the Petri net product *)

Plot.plot_product Prod1.prod1 ~filename:"prod1";;
Plot.plot_marking_graph Prod1.prod1 ~filename:"prod1.mark"

(* Unfold a product *)

open StringProductUnfolder
let u = Unfolder.unfold 10 Prod1.prod1;;
Plot.plot_unfold u ~filename:"prod1.unfold"

(* Parse an LTL formula *)

module FormulaParser = Ltllib.Parse.Make (struct
  module AP = StringPTNetProduct.Place
  module LTL = Ltllib.Ltl.Make (AP)
  let inject s = s
end)

(* p3 can be visited infinitely often *)
let phi = FormulaParser.parse "G F p3";;

(* Test the LTL formula on the Petri net *)
open String_ltl.StringNetfullsync;;

test Prod1.prod1 phi;;
