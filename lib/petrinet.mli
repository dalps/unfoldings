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
    val compare : 'a -> 'a -> int
  end

  module TransSet : Set.S
  module PlaceSet : Set.S
  module NodeSet : module type of Set.Make (Node)

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
    ((trans -> PlaceSet.t) * (trans -> PlaceSet.t)) list ->
    PlaceSet.elt list ->
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
  val add_place : PlaceSet.elt -> t -> unit
  val add_trans : TransSet.elt -> t -> unit
  val add_places : PlaceSet.t -> t -> unit
  val add_transs : TransSet.t -> t -> unit
  val add_to_trans_arc : PlaceSet.elt -> trans -> t -> unit
  val add_to_place_arc : trans -> PlaceSet.elt -> t -> unit
  val set_marking : PlaceSet.t -> t -> unit
  val preset_p : t -> PlaceSet.elt -> TransSet.t
  val postset_p : t -> PlaceSet.elt -> TransSet.t
  val nodeset_of_placeset : PlaceSet.t -> NodeSet.t
  val nodeset_of_transset : TransSet.t -> NodeSet.t
  val preset_x : t -> Node.t -> NodeSet.t
  val postset_x : t -> Node.t -> NodeSet.t
  val enables : PlaceSet.t -> trans -> t -> bool
  val fire : trans -> t -> unit
  val is_occurrence_sequence : TransSet.elt list -> t -> bool
  val fire_sequence : TransSet.elt list -> t -> unit
end

module Make (P : Set.OrderedType) (T : Set.OrderedType) :
  S
    with type place = P.t
     and type trans = T.t
     and module PlaceSet = Set.Make(P)
     and module TransSet = Set.Make(T)
