module Token : sig
  type t

  val build : Product.GlobalT.t list -> string -> t
  val history : t -> Product.GlobalT.t list
  val label : t -> string
  val compare : t -> t -> int
end

module Event : sig
  type event
  type t = E of event | Rev of t

  val event_of_t : t -> event
  val build : int -> Product.GlobalT.t list -> Product.GlobalT.t -> t
  val name : t -> int
  val history : t -> Product.GlobalT.t list
  val label : t -> Product.GlobalT.t
  val compare_event : event -> event -> int
  val compare : t -> t -> int
end

include module type of Petrinet.Make (Token) (Event)

val predecessors : Node.t -> t -> NodeSet.t
val parents_of_event : Event.t -> t -> TransSet.t
val past : Event.t -> t -> Event.t list
val past_word : Event.t -> t -> Product.GlobalT.t list
val past_conf : Event.t -> t -> TransSet.t
val past_of_preset : PlaceSet.t -> t -> Event.t list

val past_word_of_preset :
  PlaceSet.t -> t -> Product.GlobalT.t -> Product.GlobalT.t list

val place_history : Token.t -> t -> Product.GlobalT.t list
val is_predecessor : Node.t -> Node.t -> t -> bool
val is_causally_related : Node.t -> Node.t -> t -> bool
val is_conflict : Node.t -> Node.t -> t -> bool
val is_concurrent : Node.t -> Node.t -> t -> bool
val is_reachable : PlaceSet.t -> t -> bool
val union : t -> t -> t
val reversible : t -> t
