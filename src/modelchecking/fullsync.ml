open Petrilib
open Ltllib

module Make (Net : Petrinet.S) = struct
  (* atomic propositions: places or transitions? *)
  module TesterLtl = Ltl.Make (Net.Trans)
  open TesterLtl

  module Node = struct
    type t = Net.MV.t * FormulaGNBA.NumberedNba.Node.t

    let compare = compare
    let hash = Hashtbl.hash
    let equal = ( = )
  end

  module Edge = Net.ME

  module TG =
    Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

  let apset_of_transet =
    Utils.apples_of_pears (module APSet) (module Net.TransSet)

  let sync ts nba =
    let open FormulaGNBA.NumberedNba in
    let marking_graph = Net.get_marking_graph ts () in

    (* L(t) is the set of incoming transitions of marking t *)
    let label t =
      Net.MG.fold_pred_e
        (fun (_, a, _) ->
          match a with
          | `E a -> APSet.add a
          | `Def -> fun _ -> APSet.empty)
        marking_graph t APSet.empty
    in

    let g = TG.create () in
    Net.MG.iter_edges_e
      (fun (s, a, t) ->
        StateSet.iter
          (fun q ->
            StateSet.iter
              (fun p -> TG.add_edge_e g ((s, q), a, (t, p)))
              (nba.func q (label t)))
          nba.states)
      marking_graph;
    g

  let tester_of_formula ts f =
    let _ = ap_of_formula f in
    nba_of_formula (apset_of_transet (Net.transitions ts)) f

  let sync_f ts f = sync ts (tester_of_formula ts f)
end
