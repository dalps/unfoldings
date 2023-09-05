open String_product
open String_ltl

let plot_gnba g =
  StringLtl.FormulaGNBA.print_graph g ~vertex_name:string_of_formulaset
    ~edge_label:string_of_apset

let plot_nbanet n =
  StringLtl.FormulaPTNet.print_graph n ~vertex_name:label_of_node
    ~vertex_label:string_of_node

let plot_nba b =
  StringLtl.FormulaGNBA.NumberedNba.print_graph b
    ~vertex_name:name_of_numberedstate ~edge_label:string_of_apset

let plot_product p =
  StringPTNetProduct.print_graph p ~vertex_name:String_product.string_of_node
    ~vertex_label:String_product.string_of_node

let plot_unfold u =
  StringProductUnfolder.Unfolder.OccurrenceNet.print_graph u
    ~vertex_label:String_product.label_of_unfold_node
    ~vertex_name:String_product.name_of_unfold_node

let plot_test_result (r : StringProductUnfolder.Unfolder.TestResult.t) =
  StringProductUnfolder.Unfolder.OccurrenceNet.(
    print_graph r.prefix ~vertex_label:String_product.label_of_unfold_node
      ~vertex_name:String_product.name_of_unfold_node ~vertex_attrs:(fun n ->
        match n with
        | Node.T e ->
            if TransSet.mem e r.terms then
              [ `ColorWithTransparency 0x44l; `Style `Filled ]
            else []
        | Node.P p ->
            if
              TransSet.exists
                (fun t -> PlaceSet.mem p (r.prefix.postset t))
                r.terms
            then [ `ColorWithTransparency 0x44l; `Style `Filled ]
            else []))

let plot_marking_graph p =
  StringPTNetProduct.print_marking_graph p
    ~vertex_name:String_product.string_of_placeset
    ~vertex_label:String_product.string_of_placeset
    ~edge_label:String_product.string_of_globaltrans

let plot_unfold_marking_graph u =
  StringProductUnfolder.Unfolder.OccurrenceNet.print_marking_graph u
    ~vertex_name:String_product.string_of_tokenset
    ~vertex_label:String_product.string_of_tokenset
    ~edge_label:String_product.label_of_event

let plot_sync_graph net f =
  let open StringFullsync in
  let nba = tester_of_formula net f in
  let g = sync net nba in
  print_graph g ~vertex_name:name_of_sync_node ~vertex_label:name_of_sync_node
    ~vertex_attrs:(fun (_, q) ->
      if TesterLtl.FormulaGNBA.NumberedNba.StateSet.mem q nba.fin then
        [ `ColorWithTransparency 0xff000088l; `Style `Filled ]
      else [])
    ~edge_label:string_of_globaltrans

let plot_netgnba gnba =
  let open StringNetfullsync.NetGNBA in
  print_graph gnba ~vertex_name:string_of_netformulaset
    ~edge_label:string_of_globaltrans

let plot_unfoldgnba gnba =
  let open UnfoldTester.NetGNBA in
  print_graph gnba ~vertex_name:string_of_tokenformulaset
    ~edge_label:string_of_event
