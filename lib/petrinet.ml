module type S = sig
  type place
  type trans
  type t

  module Node : sig
    type t = P of place | T of trans

    val of_place : place -> t
    val of_trans : trans -> t
    val is_place : t -> bool
    val is_trans : t -> bool
    val place_of : t -> place
    val trans_of : t -> trans
    val compare : t -> t -> int
  end

  module Place : Set.OrderedType with type t = place
  module Trans : Set.OrderedType with type t = trans
  module PlaceSet : Set.S with type elt = place
  module TransSet : Set.S with type elt = trans
  module NodeSet : Set.S with type elt = Node.t

  val bottom : trans -> PlaceSet.t

  val bind_pset :
    (trans -> PlaceSet.t) -> trans -> PlaceSet.t -> trans -> PlaceSet.t

  val bind_p : (trans -> PlaceSet.t) -> trans -> place -> trans -> PlaceSet.t

  val bind_f :
    (trans -> PlaceSet.t) -> (trans -> PlaceSet.t) -> trans -> PlaceSet.t

  val ( --> ) :
    place list ->
    place list ->
    trans ->
    (trans -> PlaceSet.t) * (trans -> PlaceSet.t)

  val empty : unit -> t

  val of_lists :
    place list ->
    trans list ->
    ((trans -> PlaceSet.t) * (trans -> PlaceSet.t)) list ->
    place list ->
    t

  val of_sets :
    PlaceSet.t ->
    TransSet.t ->
    (trans -> PlaceSet.t) ->
    (trans -> PlaceSet.t) ->
    PlaceSet.t ->
    t

  val copy : t -> t
  val places : t -> PlaceSet.t
  val transitions : t -> TransSet.t
  val preset_t : t -> trans -> PlaceSet.t
  val postset_t : t -> trans -> PlaceSet.t
  val marking : t -> PlaceSet.t
  val add_place : place -> t -> unit
  val add_trans : trans -> t -> unit
  val add_places : PlaceSet.t -> t -> unit
  val add_transs : TransSet.t -> t -> unit
  val add_to_trans_arc : place -> trans -> t -> unit
  val add_to_place_arc : trans -> place -> t -> unit
  val set_marking : PlaceSet.t -> t -> unit
  val preset_p : t -> place -> TransSet.t
  val postset_p : t -> place -> TransSet.t
  val nodeset_of_placeset : PlaceSet.t -> NodeSet.t
  val nodeset_of_transset : TransSet.t -> NodeSet.t
  val preset_x : t -> Node.t -> NodeSet.t
  val postset_x : t -> Node.t -> NodeSet.t
  val enables : PlaceSet.t -> trans -> t -> bool
  val fire : trans -> t -> unit
  val is_occurrence_sequence : trans list -> t -> bool
  val fire_sequence : trans list -> t -> unit
  val is_freechoice : t -> bool
  val is_statemachine : t -> bool
  val is_marked_graph : t -> bool

  module MV : Graph.Sig.COMPARABLE
  module ME : Graph.Sig.ORDERED_TYPE_DFT with type t = [ `E of Trans.t | `Def ]

  module MG :
      module type of
        Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (MV) (ME)

  val get_marking_graph : t -> ?max_steps:int -> unit -> MG.t
end

module Make (P : Set.OrderedType) (T : Set.OrderedType) = struct
  type place = P.t
  type trans = T.t

  module Node = struct
    type t = P of P.t | T of T.t

    exception NotAPlace
    exception NotATransition

    let of_place p = P p
    let of_trans e = T e
    let is_place = function P _ -> true | T _ -> false
    let is_trans = function P _ -> false | T _ -> true
    let place_of = function P p -> p | _ -> raise NotAPlace
    let trans_of = function T e -> e | _ -> raise NotATransition
    let compare = compare
    let equal = ( = )
    let hash = Hashtbl.hash
  end

  module Edge = struct
    type t = string

    let compare = compare
    let equal = ( = )
    let default = ""
  end

  module Place = P
  module Trans = T
  module PlaceSet = Set.Make (P)
  module TransSet = Set.Make (T)
  module NodeSet = Set.Make (Node)

  type t = {
    mutable places : PlaceSet.t;
    mutable transitions : TransSet.t;
    mutable preset : T.t -> PlaceSet.t;
    mutable postset : T.t -> PlaceSet.t;
    mutable marking : PlaceSet.t;
  }

  let bottom _ = PlaceSet.empty
  let bind_pset f t pset t' = if t' = t then PlaceSet.union pset (f t) else f t'
  let bind_p f t p t' = if t' = t then PlaceSet.add p (f t) else f t'
  let bind_f f f' t = PlaceSet.union (f t) (f' t)

  let ( --> ) pre post t =
    ( bind_pset bottom t (PlaceSet.of_list pre),
      bind_pset bottom t (PlaceSet.of_list post) )

  let empty () =
    {
      places = PlaceSet.empty;
      transitions = TransSet.empty;
      preset = bottom;
      postset = bottom;
      marking = PlaceSet.empty;
    }

  let of_lists ps ts fs im =
    {
      places = PlaceSet.of_list ps;
      transitions = TransSet.of_list ts;
      preset = List.fold_right (fun f -> bind_f (fst f)) fs bottom;
      postset = List.fold_right (fun f -> bind_f (snd f)) fs bottom;
      marking = PlaceSet.of_list im;
    }

  let of_sets places transitions preset postset marking =
    { places; transitions; preset; postset; marking }

  let copy n = of_sets n.places n.transitions n.preset n.postset n.marking
  let places n = n.places
  let transitions n = n.transitions
  let preset_t n = n.preset
  let postset_t n = n.postset
  let marking n = n.marking
  let add_place p n = n.places <- PlaceSet.add p n.places
  let add_trans t n = n.transitions <- TransSet.add t n.transitions
  let add_places ps n = n.places <- PlaceSet.union ps n.places
  let add_transs ts n = n.transitions <- TransSet.union ts n.transitions
  let add_to_trans_arc p t n = n.preset <- bind_p n.preset t p
  let add_to_place_arc t p n = n.postset <- bind_p n.postset t p

  let set_marking m n =
    n.marking <- (if PlaceSet.subset m n.places then m else n.marking)

  let preset_p n p =
    TransSet.filter (fun t -> PlaceSet.mem p (n.postset t)) n.transitions

  let postset_p n p =
    TransSet.filter (fun t -> PlaceSet.mem p (n.preset t)) n.transitions

  let nodeset_of_placeset pset =
    PlaceSet.fold (fun p -> NodeSet.add (Node.of_place p)) pset NodeSet.empty

  let nodeset_of_transset tset =
    TransSet.fold (fun t -> NodeSet.add (Node.of_trans t)) tset NodeSet.empty

  let preset_x n x =
    if Node.is_place x then nodeset_of_transset (preset_p n (Node.place_of x))
    else nodeset_of_placeset (n.preset (Node.trans_of x))

  let postset_x n x =
    if Node.is_place x then nodeset_of_transset (postset_p n (Node.place_of x))
    else nodeset_of_placeset (n.postset (Node.trans_of x))

  let enables m t n = PlaceSet.subset (n.preset t) m

  let fire t n =
    if enables n.marking t n then
      set_marking
        (PlaceSet.union (PlaceSet.diff n.marking (n.preset t)) (n.postset t))
        n

  let is_occurrence_sequence ts n =
    let rec helper m = function
      | [] -> true
      | t :: ts' ->
          assert (TransSet.mem t n.transitions);
          let m' =
            PlaceSet.union (PlaceSet.diff m (n.preset t)) (n.postset t)
          in
          enables m t n && helper m' ts'
    in

    helper n.marking ts

  let fire_sequence ts n =
    assert (is_occurrence_sequence ts n);
    List.iter (fun t -> fire t n) ts

  let preset_tset n tset =
    TransSet.fold (fun t -> PlaceSet.union (n.preset t)) tset PlaceSet.empty

  let is_freechoice n =
    PlaceSet.for_all
      (fun p ->
        TransSet.cardinal (postset_p n p) = 1
        || PlaceSet.equal (preset_tset n (postset_p n p)) (PlaceSet.singleton p))
      n.places

  let is_statemachine n =
    TransSet.for_all
      (fun t ->
        let postcard = PlaceSet.cardinal (n.postset t) in
        PlaceSet.cardinal (n.preset t) = postcard && postcard = 1)
      n.transitions

  let is_marked_graph n =
    PlaceSet.for_all
      (fun p ->
        let postcard = TransSet.cardinal (postset_p n p) in
        TransSet.cardinal (preset_p n p) = postcard && postcard = 1)
      n.places

  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

  let get_graph n =
    let g = G.create () in
    TransSet.iter
      (fun t ->
        PlaceSet.iter (fun p -> G.add_edge g (P p) (T t)) (n.preset t);
        PlaceSet.iter (fun p -> G.add_edge g (T t) (P p)) (n.postset t))
      n.transitions;
    g

  let print_graph n ?(vertex_name = fun v -> string_of_int (G.V.hash v))
      ?(vertex_label = fun _ -> "") ?(vertex_attrs = fun _ -> [])
      ?(edge_attrs = fun _ -> []) ?(graph_label = "") ?(file_name = "net") () =
    let module Plotter = Graph.Graphviz.Dot (struct
      include G

      let graph_attributes _ = [ `Label graph_label ]
      let edge_attributes (_, e, _) = [ `Dir `Forward ] @ edge_attrs e
      let default_edge_attributes _ = []
      let get_subgraph _ = None

      let vertex_attributes v =
        (match v with
        | Node.P p ->
            [ `Shape `Circle; `Fixedsize true ]
            @ [
                `Label
                  (vertex_label v
                  ^ if PlaceSet.mem p n.marking then "\n&#9679;" else "");
              ]
        | T _ -> [ `Shape `Box ])
        @ vertex_attrs v

      let vertex_name v = "\"" ^ vertex_name v ^ "\""
      let default_vertex_attributes _ = []
    end) in
    let g = get_graph n in
    let file = open_out_bin (file_name ^ ".dot") in
    Plotter.output_graph file g;
    Sys.command ("neato -Tpng " ^ file_name ^ ".dot -o " ^ file_name ^ ".png")

  module MV = struct
    type t = PlaceSet.t

    let compare = PlaceSet.compare
    let equal = ( = )
    let hash = Hashtbl.hash
  end

  module ME = struct
    type t = [ `E of Trans.t | `Def ]

    let compare = compare
    let equal = ( = )
    let default = `Def
  end

  module MG = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (MV) (ME)

  let get_marking_graph n ?(max_steps = 999) () =
    let module LabelMap = Map.Make (MV) in
    let g = MG.create () in
    let m0 = n.marking in
    let l0 = LabelMap.singleton m0 `New in
    (* assumes n.marking is the initial marking *)
    let rec helper i l =
      if i > max_steps then print_endline "Exceeded step limit!"
      else
        let feasibles = LabelMap.filter (fun _ label -> label = `New) l in
        print_string (string_of_int (LabelMap.cardinal feasibles));
        let opt = LabelMap.choose_opt feasibles in
        match opt with
        | Some (m1, `New) ->
            let l' =
              TransSet.fold
                (fun t ->
                  let m2 =
                    PlaceSet.union (PlaceSet.diff m1 (n.preset t)) (n.postset t)
                  in
                  (* if not (MG.mem_edge g m1 m2) then *)
                  MG.add_edge_e g (m1, `E t, m2);
                  LabelMap.update m2 (function
                    | None -> Some `New
                    | _ as o -> o))
                (TransSet.filter (fun t -> enables m1 t n) n.transitions)
                l
            in
            helper (i + 1) (LabelMap.update m1 (fun _ -> Some `Old) l')
        | _ -> ()
    in
    helper 0 l0;
    g

  let print_marking_graph n
      ?(vertex_name = fun v -> string_of_int (MG.V.hash v))
      ?(vertex_label = fun _ -> "") ?(vertex_attrs = fun _ -> [])
      ?(edge_label = fun _ -> "") ?(edge_attrs = fun _ -> [])
      ?(graph_label = "") ?(file_name = "marking") () =
    let module Plotter = Graph.Graphviz.Neato (struct
      include MG

      let graph_attributes _ = [ `Label graph_label; `Overlap false ]

      let edge_attributes (_, e, _) =
        [
          `Label (match e with `E t -> edge_label t | `Def -> "");
          `Dir `Forward;
        ]
        @ edge_attrs e

      let default_edge_attributes _ = []
      let get_subgraph _ = None

      let vertex_attributes v =
        [ `Shape `Plaintext; `Label (vertex_label v) ] @ vertex_attrs v

      let vertex_name v = "\"" ^ vertex_name v ^ "\""
      let default_vertex_attributes _ = []
    end) in
    let g = get_marking_graph n () in
    let file = open_out_bin (file_name ^ ".dot") in
    Plotter.output_graph file g;
    Sys.command ("neato -Tpng " ^ file_name ^ ".dot -o " ^ file_name ^ ".png")
end
