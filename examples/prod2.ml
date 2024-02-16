(* Product of Fig. 2.2 p. 7 *)

open Stringnetlib.String_product.StringPTNet

let s =
  of_lists [ "s1"; "s2"; "s3"; "s4" ]
    [ "t1"; "t2"; "t3"; "t4"; "t5" ]
    [
      ([ "s1" ] --> [ "s2" ]) "t1";
      ([ "s2" ] --> [ "s4" ]) "t3";
      ([ "s1" ] --> [ "s3" ]) "t2";
      ([ "s3" ] --> [ "s4" ]) "t4";
      ([ "s4" ] --> [ "s1" ]) "t5";
    ]
    [ "s1" ]

let r =
  of_lists [ "r1"; "r2"; "r3" ] [ "u1"; "u2"; "u3" ]
    [
      ([ "r1" ] --> [ "r2" ]) "u1";
      ([ "r2" ] --> [ "r3" ]) "u2";
      ([ "r3" ] --> [ "r1" ]) "u3";
    ]
    [ "r1" ]

open Stringnetlib.String_product.StringPTNetProduct

let prod2 =
  product [ s; r ]
    [
      [ `T "t1"; `Idle ];
      [ `T "t2"; `Idle ];
      [ `T "t3"; `T "u2" ];
      [ `T "t4"; `T "u2" ];
      [ `T "t5"; `Idle ];
      [ `Idle; `T "u1" ];
      [ `Idle; `T "u3" ];
    ]

module Node = struct
  type t = P of place | T of trans

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Edge = struct
  type t = string

  let compare = compare
  let equal = ( = )
  let default = ""
end

module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

let get_graph () =
  let g = G.create () in
  PlaceSet.iter (fun p -> G.add_vertex g (P p)) prod2.places;
  TransSet.iter (fun t -> G.add_vertex g (T t)) prod2.transitions;
  TransSet.iter
    (fun t ->
      PlaceSet.iter (fun p -> G.add_edge g (P p) (T t)) (prod2.preset t);
      PlaceSet.iter (fun p -> G.add_edge g (T t) (P p)) (prod2.postset t))
    prod2.transitions;
  g

module Dot = Graph.Graphviz.Dot (struct
  include G

  let graph_attributes _ = [ `Center true; `Rankdir `LeftToRight ]
  let edge_attributes (_, e, _) = [ `Label e; `Color 4711 ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None

  let vertex_attributes v =
    match v with Node.P _ -> [ `Shape `Circle ] | T _ -> [ `Shape `Box ]

  let vertex_name v =
    match v with
    | Node.P p -> p
    | T t ->
        List.fold_right
          (fun lt acc ->
            match lt with
            | `T s -> s ^ acc
            | `Idle -> "_" ^ acc)
          t ""

  let default_vertex_attributes _ = []
end)

let print_graph () =
  let g = get_graph () in
  let file = open_out_bin "mygraph.dot" in
  let _ = Dot.output_graph file g in
  Sys.command "dot -Tpng mygraph.dot -o mygraph.png"
