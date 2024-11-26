# Unfoldings

A framework to reason about Petri nets and their unfoldings and perform model checking with LTL formulae.

This project aims to implement the great ideas and algorithms presented in the book [*Unfoldings* (J. Esparza, K. Heljanko, 2008)*](https://link.springer.com/book/10.1007/978-3-540-77426-6).

## Example

You can model-check in the toplevel with the provided [examples Petri nets](examples/):

```ocaml
open Stringnetlib;;
open String_product;;
open Examples;;

(* Plot the Petri net product *)
Plot.plot_product Prod1.prod1 ~filename:"prod1";;
Plot.plot_marking_graph Prod1.prod1 ~filename:"prod1.mark";;

(* Unfold a product *)
open StringProductUnfolder
let u = Unfolder.unfold 10 Prod1.prod1;;
Plot.plot_unfold u ~filename:"prod1.unfold"

(* This formula says: "the place p3 can be visited infinitely often" *)
let phi = FormulaParser.parse "G F p3";;

(* Test the LTL formula on the Petri net *)
open String_ltl.StringNetfullsync;;

test Prod1.prod1 phi;;
(* Ok true *)
```
