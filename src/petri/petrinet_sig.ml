open Utils

module type S = sig
  type place
  type trans
  type t

  module Node : sig
    type t = [ `P of place | `T of trans ]

    val of_place : place -> t
    val of_trans : trans -> t
    val is_place : t -> bool
    val is_trans : t -> bool
    val place_of : t -> place
    val trans_of : t -> trans
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end

  module Place : Set.OrderedType with type t = place
  module Trans : Set.OrderedType with type t = trans
  module PlaceSet : SetUtils.S with type elt = place
  module TransSet : SetUtils.S with type elt = trans
  module NodeSet : SetUtils.S with type elt = Node.t

  val bottom : trans -> PlaceSet.t

  val bind_pset :
    (trans -> PlaceSet.t) -> trans -> PlaceSet.t -> trans -> PlaceSet.t

  val bind_p : (trans -> PlaceSet.t) -> trans -> place -> trans -> PlaceSet.t

  val bind_f :
    (trans -> PlaceSet.t) -> (trans -> PlaceSet.t) -> trans -> PlaceSet.t

  val ( --> ) :
    place list -> place list -> trans -> PlaceSet.t * trans * PlaceSet.t

  val init :
    ?places:PlaceSet.t ->
    ?transitions:TransSet.t ->
    ?preset:(trans, PlaceSet.t) Hashtbl.t ->
    ?postset:(trans, PlaceSet.t) Hashtbl.t ->
    ?marking:PlaceSet.t ->
    unit ->
    t

  val of_lists :
    place list ->
    trans list ->
    (PlaceSet.t * trans * PlaceSet.t) list ->
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
  val add_edges : PlaceSet.t * trans * PlaceSet.t -> t -> unit
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

  module G : module type of Graph.Imperative.Digraph.ConcreteBidirectional (Node)

  val get_graph : t -> G.t

  module MV : Graph.Sig.COMPARABLE with type t = PlaceSet.t
  module ME : Graph.Sig.ORDERED_TYPE_DFT with type t = [ `E of Trans.t | `Def ]

  module MG :
      module type of
        Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (MV) (ME)

  val get_marking_graph : t -> ?max_steps:int -> unit -> MG.t

  module Plotter : module type of struct
    include Plotlib.Plotter.Make (G)
  end
  val get_style : t -> (module Plotter.Style) -> (module Plotter.Style)

  module MGPlotter : module type of struct
    include Plotlib.Plotter.Make (MG)
  end
  val get_mgstyle : (module MGPlotter.Style) -> (module MGPlotter.Style)
end

module type Make = functor (P : Set.OrderedType) (T : Set.OrderedType) ->
  S (* expose place and trans types *)
    with type place = P.t
     and type trans = T.t
