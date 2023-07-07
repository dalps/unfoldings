module type S = sig
  type place1
  type trans1

  module Token : sig
    type t = { history : trans1 list; label : place1 }

    val build : trans1 list -> place1 -> t
    val history : t -> trans1 list
    val label : t -> place1
    val compare : t -> t -> int
  end

  module Event : sig
    type event = { name : int; history : trans1 list; label : trans1 }
    type t = E of event | Rev of t

    val event_of_t : t -> event
    val build : int -> trans1 list -> trans1 -> t
    val name : t -> int
    val history : t -> trans1 list
    val label : t -> trans1
    val compare_event : event -> event -> int
    val compare : t -> t -> int
  end

  include module type of Petrinet.Make (Token) (Event)

  val predecessors : NodeSet.elt -> t -> NodeSet.t
  val parents_of_event : trans -> t -> TransSet.t
  val past : trans -> t -> TransSet.elt list
  val past_word : trans -> t -> trans1 list
  val past_conf : TransSet.elt -> t -> TransSet.t
  val past_of_preset : PlaceSet.t -> t -> TransSet.elt list
  val past_word_of_preset : PlaceSet.t -> t -> trans1 -> trans1 list
  val place_history : PlaceSet.elt -> t -> trans1 list
  val is_predecessor : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_causally_related : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_conflict : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_concurrent : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_reachable : PlaceSet.t -> t -> bool
  val union : t -> t -> t
  val reversible : t -> t
end

module Make (P : Set.OrderedType) (T : Set.OrderedType) :
  S with type place1 = P.t and type trans1 = T.t
