let wrap_name = Printf.sprintf "\"%s\""

module Make (G : Graph.Sig.G) = struct
  module type Style = sig
    open Graph.Graphviz.NeatoAttributes

    val graph_label : string
    val graph_attributes : G.t -> graph list

    val default_vertex_attributes : G.t -> vertex list
    val vertex_name : G.V.t -> string (* unique vertex identifiers *)
    val vertex_label : G.V.t -> string (* non-unique vertex names *)
    val vertex_attributes : G.V.t -> vertex list

    val default_edge_attributes : G.t -> edge list
    val edge_label : G.E.t -> string
    val edge_attributes : G.E.t -> edge list
    val get_subgraph : G.V.t -> subgraph option
  end

  module DefaultStyle : Style = struct
    let graph_label = ""
    let graph_attributes _ = []

    let default_vertex_attributes _ = []
    let vertex_name v = string_of_int (G.V.hash v)
    let vertex_label _ = ""
    let vertex_attributes _ = []

    let default_edge_attributes _ = []
    let edge_label _ = ""
    let edge_attributes _ = []
    let get_subgraph _ = None
  end

  let extend_style (module S : Style) (module S' : Style) =
    (module struct
      include S'
      let graph_attributes g = S.graph_attributes g @ S'.graph_attributes g

      let default_vertex_attributes v =
        S.default_vertex_attributes v @ S'.default_vertex_attributes v
      let vertex_attributes v = S.vertex_attributes v @ S'.vertex_attributes v

      let default_edge_attributes e =
        S.default_edge_attributes e @ S'.default_edge_attributes e
      let edge_attributes e = S.edge_attributes e @ S'.edge_attributes e
    end : Style)

  let print_graph (module S : Style) g ~filename =
    let module Plotter = Graph.Graphviz.Neato (struct
      include G
      include S
      let graph_attributes g = S.graph_attributes g @ [ `Label graph_label ]

      let vertex_name v = wrap_name (vertex_name v)
      let vertex_attributes v =
        S.vertex_attributes v @ [ `Label (S.vertex_label v) ]

      let edge_attributes v = S.edge_attributes v @ [ `Label (S.edge_label v) ]
    end) in
    let f = open_out_bin (Printf.sprintf "%s.dot" filename) in
    Plotter.output_graph f g;
    Sys.command
      (Printf.sprintf "neato -Tpng %s.dot -o %s.png" filename filename)
end
