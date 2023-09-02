open String_product
open String_ltl

let plot_gnba g =
  StringLtl.FormulaGNBA.print_graph g
    ~vertex_name:String_ltl.string_of_formulaset
    ~edge_label:String_ltl.string_of_apset

let plot_nbanet n =
  StringLtl.FormulaPTNet.print_graph n ~vertex_name:String_ltl.label_of_node
    ~vertex_label:String_ltl.string_of_node

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
