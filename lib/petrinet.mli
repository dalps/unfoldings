module Make :
  functor (P : Set.OrderedType) (T : Set.OrderedType) -> sig
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

    val ( -->@ ) : T.t -> P.t -> Flow.t
    val ( @--> ) : P.t -> T.t -> Flow.t

    type t

    val empty : unit -> t
    val of_lists : P.t list -> T.t list -> Flow.t list -> P.t list -> t
    val of_sets : Set.Make(P).t -> Set.Make(T).t -> Set.Make(Flow).t -> Set.Make(P).t -> t
    val copy : t -> t
    val places : t -> Set.Make(P).t
    val transitions : t -> Set.Make(T).t
    val flow : t -> Set.Make(Flow).t
    val marking : t -> Set.Make(P).t
    val add_place : P.t -> t -> unit
    val add_trans : T.t -> t -> unit
    val add_places : Set.Make(P).t -> t -> unit
    val add_transs : T.t list -> t -> unit
    val add_to_place_arc : T.t -> P.t -> t -> unit
    val add_to_trans_arc : P.t -> T.t -> t -> unit
    val set_marking : Set.Make(P).t -> t -> unit
    val nodes_of_places : Set.Make(P).t -> Set.Make(Node).t 
    val nodes_of_transs : Set.Make(T).t -> Set.Make(Node).t 
    val inputs_of : Node.t -> t -> Set.Make(Node).t
    val outputs_of : Node.t -> t -> Set.Make(Node).t
    val inputs_of_place : P.t -> t -> Set.Make(T).t
    val outputs_of_place : P.t -> t -> Set.Make(T).t
    val inputs_of_trans : T.t -> t -> Set.Make(P).t
    val outputs_of_trans : T.t -> t -> Set.Make(P).t
    val enables : Set.Make(P).t -> T.t -> t -> bool
    val fire : T.t -> t -> unit
    val is_occurrence_sequence : T.t list -> t -> bool
    val fire_sequence : T.t list -> t -> unit
  end