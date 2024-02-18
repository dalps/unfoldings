open Ltllib
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
        | `T e ->
            if TransSet.mem e r.terms then
              [ `ColorWithTransparency 0x44l; `Style `Filled ]
            else []
        | `P p ->
            if
              TransSet.exists
                (fun t -> PlaceSet.mem p ((postset_t r.prefix) t))
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

let plot_net_gnba gnba =
  let open StringNetfullsync.NetGNBA in
  print_graph gnba ~vertex_name:string_of_netformulaset
    ~edge_label:string_of_globaltrans

let plot_unfold_gnba gnba =
  let open UnfoldTester.NetGNBA in
  print_graph gnba ~vertex_name:string_of_tokenformulaset
    ~edge_label:label_of_event

let plot_product_tester ?(stutter = false) net f =
  let notf = Ltl.Not f in
  let open StringNetfullsync in
  let nba = nba_of_formula net notf in
  let prd =
    if stutter then sync ~stutter:(is_stuttering net notf) net nba
    else sync net nba
  in
  SyncNet.print_graph prd ~vertex_name:string_of_netsyncnode
    ~vertex_label:string_of_netsyncnode ~vertex_attrs:(function
    | `T (_, u) -> (
        match u with
        | `U e ->
            if NetGNBA.NumberedNba.StateSet.mem e.tgt nba.fin then
              [ `ColorWithTransparency 0xFF000088l; `Style `Filled ]
            else []
        | _ -> [])
    | `P p -> (
        match p with
        | `NbaP b ->
            if NetGNBA.NumberedNba.StateSet.mem b nba.fin then
              [ `ColorWithTransparency 0xFF000044l; `Style `Filled ]
            else []
        | `NetP _ -> []))

let plot_unfold_tester ?(stutter = false) u f =
  let notf = Ltl.Not f in
  let open UnfoldTester in
  let nba = nba_of_formula u notf in
  let prd =
    if stutter then sync ~stutter:(is_stuttering u notf) u nba else sync u nba
  in
  SyncNet.print_graph prd ~vertex_name:string_of_unfoldsyncnode
    ~vertex_label:string_of_unfoldsyncnode ~vertex_attrs:(function
    | `T (_, u) -> (
        match u with
        | `U e ->
            if NetGNBA.NumberedNba.StateSet.mem e.tgt nba.fin then
              [ `ColorWithTransparency 0xFF000088l; `Style `Filled ]
            else []
        | _ -> [])
    | `P p -> (
        match p with
        | `NbaP b ->
            if NetGNBA.NumberedNba.StateSet.mem b nba.fin then
              [ `ColorWithTransparency 0xFF000044l; `Style `Filled ]
            else []
        | `NetP _ -> []))

let plot_custom_unfold_tester ?(stutter = false) u f nba =
  let notf = Ltl.Not f in
  let open UnfoldTester in
  let prd =
    if stutter then sync ~stutter:(is_stuttering u notf) u nba else sync u nba
  in
  SyncNet.print_graph prd ~vertex_name:string_of_unfoldsyncnode
    ~vertex_label:string_of_unfoldsyncnode ~vertex_attrs:(function
    | `T (_, u) -> (
        match u with
        | `U e ->
            if NetGNBA.NumberedNba.StateSet.mem e.tgt nba.fin then
              [ `ColorWithTransparency 0xFF000088l; `Style `Filled ]
            else []
        | _ -> [])
    | `P p -> (
        match p with
        | `NbaP b ->
            if NetGNBA.NumberedNba.StateSet.mem b nba.fin then
              [ `ColorWithTransparency 0xFF000044l; `Style `Filled ]
            else []
        | `NetP _ -> []))
