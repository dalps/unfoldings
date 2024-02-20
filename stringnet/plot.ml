open String_product
open String_ltl
open Plotlib

let terminal_color = 0x044l
let final_trans_color = 0xFF000088l
let final_state_color = 0xFF000044l

let plot_gnba g =
  StringLtl.FormulaGNBA.print_graph g ~vertex_name:string_of_formulaset
    ~edge_label:string_of_apset

let plot_nbanet n =
  let module P = Plotter.Make (StringLtl.FormulaPTNet.G) in
  P.print_graph
    (module struct
      include (val StringLtl.FormulaPTNet.get_style n)
      let vertex_name = String_ltl.label_of_node
      let vertex_label = String_ltl.string_of_node
    end)
    (StringLtl.FormulaPTNet.get_graph n)

let plot_nba b =
  StringLtl.FormulaGNBA.NumberedNba.print_graph b
    ~vertex_name:name_of_numberedstate ~edge_label:string_of_apset

let plot_product p =
  let module P = Plotter.Make (StringPTNetProduct.G) in
  P.print_graph
    (module struct
      include (val StringPTNetProduct.get_style p)
      let vertex_label = String_product.string_of_node
      let vertex_name = String_product.string_of_node
    end)
    (StringPTNetProduct.get_graph p)

let plot_unfold u =
  let module P = Plotter.Make (StringProductUnfolder.Unfolder.OccurrenceNet.G) in
  StringProductUnfolder.Unfolder.OccurrenceNet.(
    P.print_graph
      (module struct
        include (val StringProductUnfolder.Unfolder.OccurrenceNet.get_style u)
        let vertex_label v =
          Printf.sprintf "%s%s" (label_of_unfold_node v) (vertex_label v)
        let vertex_name v = String_product.name_of_unfold_node v
      end)
      (get_graph u))

let plot_test_result (r : StringProductUnfolder.Unfolder.TestResult.t) =
  let module P = Plotter.Make (StringProductUnfolder.Unfolder.OccurrenceNet.G) in
  StringProductUnfolder.Unfolder.OccurrenceNet.(
    P.print_graph
      (module struct
        include (val get_style r.prefix)

        let vertex_label v =
          Printf.sprintf "%s%s" (label_of_unfold_node v) (vertex_label v)
        let vertex_name = name_of_unfold_node
        let vertex_attributes v =
          (match v with
          | `T e when TransSet.mem e r.terms ->
              [ `ColorWithTransparency terminal_color; `Style `Filled ]
          | `P p
            when TransSet.exists
                   (fun t -> PlaceSet.mem p ((postset_t r.prefix) t))
                   r.terms ->
              [ `ColorWithTransparency terminal_color; `Style `Filled ]
          | _ -> [])
          @ vertex_attributes v
      end)
      (get_graph r.prefix))

let plot_marking_graph p =
  StringPTNetProduct.(
    MGPlotter.print_graph
      (module struct
        include MGStyle
        let vertex_name = string_of_placeset
        let vertex_label = string_of_placeset
        let edge_label (_, e, _) =
          match e with
          | `E t -> String_product.string_of_globaltrans t
          | `Def -> ""
      end)
      (get_marking_graph p ()))

let plot_unfold_marking_graph u =
  StringProductUnfolder.Unfolder.OccurrenceNet.(
    MGPlotter.print_graph
      (module struct
        include MGStyle
        let vertex_name = string_of_tokenset
        let vertex_label = string_of_tokenset
        let edge_label (_, e, _) =
          match e with
          | `E t -> String_product.label_of_event t
          | `Def -> ""
      end)
      (get_marking_graph u ()))

let plot_net_gnba gnba =
  let open StringNetfullsync.NetGNBA in
  print_graph gnba ~vertex_name:string_of_netformulaset
    ~edge_label:string_of_globaltrans

let plot_unfold_gnba gnba =
  let open UnfoldTester.NetGNBA in
  print_graph gnba ~vertex_name:string_of_tokenformulaset
    ~edge_label:label_of_event

let plot_product_tester prd nba =
  let open StringNetfullsync in
  let module P = Plotter.Make (SyncNet.G) in
  P.print_graph
    (module struct
      include (val SyncNet.get_style prd)
      let vertex_name = string_of_netsyncnode
      let vertex_label = string_of_netsyncnode
      let vertex_attributes v =
        NetGNBA.NumberedNba.(
          match v with
          | `T (_, `U e) when NetGNBA.NumberedNba.StateSet.mem e.tgt nba.fin ->
              [ `ColorWithTransparency final_trans_color; `Style `Filled ]
          | `P (`NbaP b) when NetGNBA.NumberedNba.StateSet.mem b nba.fin ->
              [ `ColorWithTransparency final_state_color; `Style `Filled ]
          | _ -> [])
        @ vertex_attributes v
    end)
    (SyncNet.get_graph prd)

let plot_unfold_tester prd nba =
  let open UnfoldTester in
  let module P = Plotter.Make (SyncNet.G) in
  P.print_graph
    (module struct
      include (val SyncNet.get_style prd)
      let vertex_name = string_of_unfoldsyncnode
      let vertex_label = string_of_unfoldsyncnode
      let vertex_attributes v =
        NetGNBA.NumberedNba.(
          match v with
          | `T (_, `U e) when NetGNBA.NumberedNba.StateSet.mem e.tgt nba.fin ->
              [ `ColorWithTransparency final_trans_color; `Style `Filled ]
          | `P (`NbaP b) when NetGNBA.NumberedNba.StateSet.mem b nba.fin ->
              [ `ColorWithTransparency final_state_color; `Style `Filled ]
          | _ -> [])
        @ vertex_attributes v
    end)
    (SyncNet.get_graph prd)
