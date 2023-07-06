module Make : functor (P : Set.OrderedType) (T : Set.OrderedType) -> sig
  module Token : sig
    type t = { history : T.t list; label : P.t }

    val build : T.t list -> P.t -> t
    val history : t -> T.t list
    val label : t -> P.t
    val compare : t -> t -> int
  end

  module Event : sig
    type event = { name : int; history : T.t list; label : T.t }
    type t = E of event | Rev of t

    val event_of_t : t -> event
    val build : int -> T.t list -> T.t -> t
    val name : t -> int
    val history : t -> T.t list
    val label : t -> T.t
    val compare_event : event -> event -> int
    val compare : t -> t -> int
  end

  include module type of Petrinet.Make (Token) (Event)

  val predecessors : NodeSet.elt -> t -> NodeSet.t
  val parents_of_event : trans -> t -> TransSet.t
  val past : trans -> t -> TransSet.elt list
  val past_word : trans -> t -> T.t list
  val past_conf : TransSet.elt -> t -> TransSet.t
  val past_of_preset : PlaceSet.t -> t -> TransSet.elt list
  val past_word_of_preset : PlaceSet.t -> t -> T.t -> T.t list
  val place_history : PlaceSet.elt -> t -> T.t list
  val is_predecessor : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_causally_related : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_conflict : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_concurrent : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_reachable : PlaceSet.t -> t -> bool
  val union : t -> t -> t
  val reversible : t -> t
end
