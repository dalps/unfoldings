module Make (Net : Petrinet.S) = struct
  (* atomic propositions: places or transitions? *)
  module TesterLtl = Ltl.Make (Net.Trans)

  module Node = struct
    type t = Net.MV.t * TesterLtl.FormulaGNBA.NumberedNba.Node.t

    let compare = compare
    let hash = Hashtbl.hash
    let equal = ( = )
  end

  module Edge = Net.ME

  module TG =
    Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

  let apset_of_transet tset =
    Net.TransSet.fold
      (fun t -> TesterLtl.APSet.add t)
      tset TesterLtl.APSet.empty

  let sync ts nba =
    let open TesterLtl.FormulaGNBA.NumberedNba in
    let marking_graph = Net.get_marking_graph ts () in

    (* L(t) is the set of incoming transitions of marking t *)
    let label t =
      Net.MG.fold_pred_e
        (fun (_, a, _) ->
          match a with
          | `E a -> TesterLtl.APSet.add a
          | `Def -> fun _ -> TesterLtl.APSet.empty)
        marking_graph t TesterLtl.APSet.empty
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
    let _ = TesterLtl.ap_of_formula f in
    TesterLtl.nba_of_formula (apset_of_transet (Net.transitions ts)) f

  let sync_f ts f = sync ts (tester_of_formula ts f)

  let print_graph g ?(vertex_name = fun v -> string_of_int (TG.V.hash v))
      ?(vertex_label = fun _ -> "") ?(vertex_attrs = fun _ -> [])
      ?(edge_label = fun _ -> "") ?(edge_attrs = fun _ -> [])
      ?(graph_label = "") ?(file_name = "mygraph") () =
    let module Plotter = Graph.Graphviz.Neato (struct
      include TG

      let graph_attributes _ =
        [ `Label graph_label; `Center true; `Margin (1.0, 1.0); `Overlap false ]

      let edge_attributes (_, e, _) =
        [
          `Label (match e with `E t -> edge_label t | _ -> ""); `Dir `Forward;
        ]
        @ edge_attrs e

      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_attributes v = [ `Label (vertex_label v) ] @ vertex_attrs v
      let vertex_name v = "\"" ^ vertex_name v ^ "\""
      let default_vertex_attributes _ = []
    end) in
    let file = open_out_bin (file_name ^ ".dot") in
    Plotter.output_graph file g;
    Sys.command ("neato -Tpng " ^ file_name ^ ".dot -o " ^ file_name ^ ".png")
end

module MakeEH (Net : Petrinet.S) = struct
  (* atomic propositions: places *)
  module TesterLtl = Ltl.Make (Net.Place)

  module Node = struct
    type t = Net.MV.t * TesterLtl.FormulaGNBA.NumberedNba.Node.t

    let compare = compare
    let hash = Hashtbl.hash
    let equal = ( = )
  end

  module Edge = Net.ME

  module TG =
    Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

  let apset_of_placeset tset =
    Net.PlaceSet.fold
      (fun t -> TesterLtl.APSet.add t)
      tset TesterLtl.APSet.empty

  let placeset_of_apset apset =
    TesterLtl.APSet.fold (fun t -> Net.PlaceSet.add t) apset Net.PlaceSet.empty

  let f_state_set s f =
    TesterLtl.APSet.inter (apset_of_placeset s) (TesterLtl.ap_of_formula f)

  let f_state_list s f =
    let ap_of_f = TesterLtl.ap_of_formula f in
    List.fold_right
      (fun si ->
        if TesterLtl.APSet.mem si ap_of_f then ( @ ) [ `P si ]
        else ( @ ) [ `Bottom ])
      s []

  let f_step_set_of (m, t, m') f = (f_state_set m f, t, f_state_set m' f)
  let f_step_list_of (m, t, m') f = (f_state_list m f, t, f_state_list m' f)

  let is_f_step (r, t, r') mg f =
    Net.MG.fold_edges_e
      (fun (m, u, m') b ->
        let n, n' = (f_state_set m f, f_state_set m' f) in
        (match u with `E u -> Net.Trans.compare t u = 0 | `Def -> false)
        && TesterLtl.APSet.equal n r
        && TesterLtl.APSet.equal n' r'
        || b)
      mg false

  let rec is_f_occurrence_sequence h mg f =
    match h with
    | [] -> true
    | [ a ] ->
        Net.MG.fold_edges_e
          (fun (_, a', _) b ->
            (match a' with
            | `E a' -> Net.Trans.compare a a' = 0
            | `Def -> false)
            || b)
          mg false
    | a :: b :: ts ->
        Net.MG.fold_edges_e
          (fun (m, a', n) acc ->
            Net.MG.fold_edges_e
              (fun (x, b', y) ->
                ( || )
                  (match (a', b') with
                  | `E a', `E b' ->
                      Net.Trans.compare a a' = 0
                      && Net.Trans.compare b b' = 0
                      &&
                      let ((_, _, r1) as fstep), ((r1', _, _) as fstep') =
                        (f_step_set_of (m, a', n) f, f_step_set_of (x, b', y) f)
                      in
                      is_f_step fstep mg f && is_f_step fstep' mg f
                      && TesterLtl.APSet.equal r1 r1'
                  | _ -> false))
              mg acc)
          mg false
        && is_f_occurrence_sequence (b :: ts) mg f

  module NetGNBA = Gnba.Make (TesterLtl.FormulaSet) (Net.Trans)
  open NetGNBA

  let apset_of_transset tset =
    Net.TransSet.fold
      (fun t -> NetGNBA.AlphaSet.add t)
      tset NetGNBA.AlphaSet.empty

  let gnba_of_formula net f =
    let open TesterLtl in
    let f = expand f in
    let mg = Net.get_marking_graph net () in
    let cl = closure f in
    let apset = ap_of_formula f in
    let states = elementary_sets f in
    let is_f = f_state_set (Net.marking net) f in
    print_endline "";
    print_int (APSet.cardinal is_f);
    print_endline "";
    NetGNBA.of_sets states
      (apset_of_transset (Net.transitions net))
      (fun b a ->
        PowerFormulaSet.filter
          (fun b' ->
            (* condition (6) *)
            let r, r' =
              ( APSet.inter (ap_of_formulaset b) apset,
                APSet.inter (ap_of_formulaset b') apset )
            in
            is_f_step (r, a, r') mg f
            (* conditions (3)-(4) *)
            && FormulaSet.for_all
                 (function
                   | X g' as g -> FormulaSet.mem g b <=> FormulaSet.mem g' b'
                   | U (g1, g2) as g ->
                       FormulaSet.mem g b
                       <=> (FormulaSet.mem g2 b
                           || (FormulaSet.mem g1 b && FormulaSet.mem g b'))
                   | _ -> true)
                 cl)
          states)
      (PowerFormulaSet.filter
         (fun b ->
           (* condition (1) *)
           FormulaSet.mem f b
           (* condition (2) *)
           && APSet.equal (ap_of_formulaset b) is_f)
         states)
      (* condition (5) is encoded in the acceptance sets *)
      (FormulaSet.fold
         (function
           | U (_, g2) as g ->
               PowerStateSet.add
                 (PowerFormulaSet.filter
                    (fun b -> (not (FormulaSet.mem g b)) || FormulaSet.mem g2 b)
                    states)
           | _ -> PowerStateSet.union PowerStateSet.empty)
         cl PowerStateSet.empty)
end
