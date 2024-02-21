open String_product
open String_ltl

let terminal_color = 0x044l
let final_trans_color = 0xFF000088l
let final_state_color = 0xFF000044l

let plot_gnba g =
  let open StringLtl.FormulaGNBA in
  Plotter.print_graph
    (get_style g
       (module struct
         include Plotter.DefaultStyle
         let vertex_name = string_of_formulaset
         let edge_label (_, e, _) =
           match e with
           | `AP ap -> string_of_apset ap
           | `Def -> ""
       end))
    (get_graph g)

let plot_nbanet n =
  let open StringLtl.FormulaPTNet in
  Plotter.print_graph
    (get_style n
       (module struct
         include Plotter.DefaultStyle
         let vertex_name = String_ltl.label_of_node
         let vertex_label = String_ltl.string_of_node
       end))
    (get_graph n)

let plot_nba b =
  let open StringLtl.FormulaGNBA.NumberedNba in
  Plotter.print_graph
    (get_style b
       (module struct
         include Plotter.DefaultStyle
         let vertex_name = name_of_numberedstate
         let edge_label (_, e, _) =
           match e with
           | `AP ap -> string_of_apset ap
           | `Def -> ""
       end))
    (get_graph b)

let plot_product p =
  let open StringPTNetProduct in
  Plotter.print_graph
    (get_style p
       (module struct
         include Plotter.DefaultStyle
         let vertex_label = String_product.string_of_node
         let vertex_name = String_product.string_of_node
       end))
    (get_graph p)

let plot_unfold u =
  let open StringProductUnfolder.Unfolder.OccurrenceNet in
  Plotter.print_graph
    (get_style u
       (module struct
         include Plotter.DefaultStyle
         let vertex_label = label_of_unfold_node
         let vertex_name = name_of_unfold_node
       end))
    (get_graph u)

let plot_test_result (r : StringProductUnfolder.Unfolder.TestResult.t) =
  let open StringProductUnfolder.Unfolder.OccurrenceNet in
  Plotter.print_graph
    (get_style r.prefix
       (module struct
         include Plotter.DefaultStyle
         let vertex_label = label_of_unfold_node
         let vertex_name = name_of_unfold_node
         let vertex_attributes v =
           match v with
           | `T e when TransSet.mem e r.terms ->
               [ `ColorWithTransparency terminal_color; `Style `Filled ]
           | `P p
             when TransSet.exists
                    (fun t -> PlaceSet.mem p ((postset_t r.prefix) t))
                    r.terms ->
               [ `ColorWithTransparency terminal_color; `Style `Filled ]
           | _ -> []
       end))
    (get_graph r.prefix)

let plot_marking_graph p =
  let open StringPTNetProduct in
  MGPlotter.print_graph
    (module struct
      include MGStyle
      let vertex_name = string_of_placeset
      let vertex_label = string_of_placeset
      let edge_label (_, e, _) =
        match e with
        | `E t -> string_of_globaltrans t
        | `Def -> ""
    end)
    (get_marking_graph p ())

let plot_unfold_marking_graph u =
  let open StringProductUnfolder.Unfolder.OccurrenceNet in
  MGPlotter.print_graph
    (module struct
      include MGStyle
      let vertex_name = string_of_tokenset
      let vertex_label = string_of_tokenset
      let edge_label (_, e, _) =
        match e with
        | `E t -> label_of_event t
        | `Def -> ""
    end)
    (get_marking_graph u ())

let plot_net_gnba gnba =
  let open StringNetfullsync.NetGNBA in
  Plotter.print_graph
    (get_style gnba
       (module struct
         include Plotter.DefaultStyle
         let vertex_name = string_of_netformulaset
         let edge_label (_, x, _) =
           match x with
           | `AP ap -> string_of_globaltrans ap
           | `Def -> ""
       end))
    (get_graph gnba)

let plot_unfold_gnba gnba =
  let open UnfoldTester.NetGNBA in
  Plotter.print_graph
    (get_style gnba
       (module struct
         include Plotter.DefaultStyle
         let vertex_name = string_of_tokenformulaset
         let edge_label (_, x, _) =
           match x with
           | `AP ap -> label_of_event ap
           | `Def -> ""
       end))
    (get_graph gnba)

let plot_product_tester prd nba =
  let open StringNetfullsync in
  let open SyncNet in
  Plotter.print_graph
    (get_style prd
       (module struct
         include Plotter.DefaultStyle
         let vertex_name = string_of_netsyncnode
         let vertex_label = string_of_netsyncnode
         let vertex_attributes v =
           NetGNBA.NumberedNba.(
             match v with
             | `T (_, `U e) when NetGNBA.NumberedNba.StateSet.mem e.tgt nba.fin
               ->
                 [ `ColorWithTransparency final_trans_color; `Style `Filled ]
             | `P (`NbaP b) when NetGNBA.NumberedNba.StateSet.mem b nba.fin ->
                 [ `ColorWithTransparency final_state_color; `Style `Filled ]
             | _ -> [])
       end))
    (get_graph prd)

let plot_unfold_tester prd nba =
  let open UnfoldTester in
  let open SyncNet in
  Plotter.print_graph
    (get_style prd
       (module struct
         include Plotter.DefaultStyle
         let vertex_name = string_of_unfoldsyncnode
         let vertex_label = string_of_unfoldsyncnode
         let vertex_attributes v =
           NetGNBA.NumberedNba.(
             match v with
             | `T (_, `U e) when NetGNBA.NumberedNba.StateSet.mem e.tgt nba.fin
               ->
                 [ `ColorWithTransparency final_trans_color; `Style `Filled ]
             | `P (`NbaP b) when NetGNBA.NumberedNba.StateSet.mem b nba.fin ->
                 [ `ColorWithTransparency final_state_color; `Style `Filled ]
             | _ -> [])
       end))
    (get_graph prd)
