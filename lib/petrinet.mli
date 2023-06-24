module Make :
  functor (P : Set.OrderedType) (T : Set.OrderedType) -> sig
    type t

    module Node : sig
      type t
      
      val of_place : P.t -> t
      val of_trans : T.t -> t
      val is_place : t -> bool
      val is_trans : t -> bool
      val place_of : t -> P.t
      val trans_of : t -> T.t
      val compare : t -> t -> int
    end

    module Flow : sig
      type t

      val to_place : T.t -> P.t -> t
      val to_trans : P.t -> T.t -> t
      val source : t -> Node.t
      val target : t -> Node.t
      val target_trans : t -> T.t
      val source_trans : t -> T.t
      val target_place : t -> P.t
      val source_place : t -> P.t
      val compare : t -> t -> int
    end

    module PlaceSet : module type of Set.Make(P)
    module TransSet : module type of Set.Make(T)
    module NodeSet : module type of Set.Make(Node)
    module FlowSet : module type of Set.Make(Flow)

    val ( -->@ ) : T.t -> P.t -> Flow.t
    val ( @--> ) : P.t -> T.t -> Flow.t
    val empty : unit -> t
    val of_lists : P.t list -> T.t list -> Flow.t list -> P.t list -> t
    val of_sets : PlaceSet.t -> TransSet.t -> FlowSet.t -> PlaceSet.t -> t
    val copy : t -> t
    val places : t -> PlaceSet.t
    val transitions : t -> TransSet.t
    val flow : t -> FlowSet.t
    val marking : t -> PlaceSet.t
    val add_place : P.t -> t -> unit
    val add_trans : T.t -> t -> unit
    val add_places : PlaceSet.t -> t -> unit
    val add_transs : T.t list -> t -> unit
    val add_to_place_arc : T.t -> P.t -> t -> unit
    val add_to_trans_arc : P.t -> T.t -> t -> unit
    val set_marking : PlaceSet.t -> t -> unit
    val nodes_of_places : PlaceSet.t -> NodeSet.t 
    val nodes_of_transs : TransSet.t -> NodeSet.t 
    val inputs_of : Node.t -> t -> NodeSet.t
    val outputs_of : Node.t -> t -> NodeSet.t
    val inputs_of_place : P.t -> t -> TransSet.t
    val outputs_of_place : P.t -> t -> TransSet.t
    val inputs_of_trans : T.t -> t -> PlaceSet.t
    val outputs_of_trans : T.t -> t -> PlaceSet.t
    val enables : PlaceSet.t -> T.t -> t -> bool
    val fire : T.t -> t -> unit
    val is_occurrence_sequence : T.t list -> t -> bool
    val fire_sequence : T.t list -> t -> unit
  end