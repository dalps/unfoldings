module Make : functor (P : Set.OrderedType) (T : Set.OrderedType) -> sig
  module Node : sig
    type t = P of P.t | T of T.t

    exception NotAPlace
    exception NotATransition

    val of_place : P.t -> t
    val of_trans : T.t -> t
    val is_place : t -> bool
    val is_trans : t -> bool
    val place_of : t -> P.t
    val trans_of : t -> T.t
    val compare : 'a -> 'a -> int
  end

  module PlaceSet : module type of Set.Make (P)
  module TransSet : module type of Set.Make (T)
  module NodeSet : module type of Set.Make (Node)

  type t

  val bottom : 'a -> PlaceSet.t
  val bind_pset : ('a -> PlaceSet.t) -> 'a -> PlaceSet.t -> 'a -> PlaceSet.t
  val bind_p : ('a -> PlaceSet.t) -> 'a -> PlaceSet.elt -> 'a -> PlaceSet.t
  val bind_f : ('a -> PlaceSet.t) -> ('a -> PlaceSet.t) -> 'a -> PlaceSet.t

  val ( --> ) :
    PlaceSet.elt list ->
    PlaceSet.elt list ->
    'a ->
    ('a -> PlaceSet.t) * ('a -> PlaceSet.t)

  val empty : unit -> t

  val of_lists :
    PlaceSet.elt list ->
    TransSet.elt list ->
    ((T.t -> PlaceSet.t) * (T.t -> PlaceSet.t)) list ->
    PlaceSet.elt list ->
    t

  val of_sets :
    PlaceSet.t ->
    TransSet.t ->
    (T.t -> PlaceSet.t) ->
    (T.t -> PlaceSet.t) ->
    PlaceSet.t ->
    t

  val copy : t -> t
  val places : t -> PlaceSet.t
  val transitions : t -> TransSet.t
  val preset_t : t -> T.t -> PlaceSet.t
  val postset_t : t -> T.t -> PlaceSet.t
  val marking : t -> PlaceSet.t
  val add_place : PlaceSet.elt -> t -> unit
  val add_trans : TransSet.elt -> t -> unit
  val add_places : PlaceSet.t -> t -> unit
  val add_transs : TransSet.t -> t -> unit
  val add_to_trans_arc : PlaceSet.elt -> T.t -> t -> unit
  val add_to_place_arc : T.t -> PlaceSet.elt -> t -> unit
  val set_marking : PlaceSet.t -> t -> unit
  val preset_p : t -> PlaceSet.elt -> TransSet.t
  val postset_p : t -> PlaceSet.elt -> TransSet.t
  val nodeset_of_placeset : PlaceSet.t -> NodeSet.t
  val nodeset_of_transset : TransSet.t -> NodeSet.t
  val preset_x : t -> Node.t -> NodeSet.t
  val postset_x : t -> Node.t -> NodeSet.t
  val enables : PlaceSet.t -> T.t -> t -> bool
  val fire : T.t -> t -> unit
  val is_occurrence_sequence : TransSet.elt list -> t -> bool
  val fire_sequence : TransSet.elt list -> t -> unit
end
