module Place :
  sig
    type t = string (* not abstract for now *)
    val compare : 'a -> 'a -> int
  end

module Event :
  sig
    type t = string (* not abstract for now *)
    val compare : 'a -> 'a -> int
  end

module Node :
  sig
    type t
    val compare : 'a -> 'a -> int
    val is_place : t -> bool
    val is_event : t -> bool
    val of_place : Place.t -> t
    val of_event : Event.t -> t
  end

module Flow :
  sig
    type t
    val compare : 'a -> 'a -> int
    val to_place : Event.t -> Place.t -> t
    val to_event : Place.t -> Event.t -> t
  end

val (-->@) : Event.t -> Place.t -> Flow.t
val (@-->) : Place.t -> Event.t -> Flow.t

type t
val empty : unit -> t
val build : Place.t list -> Event.t list -> Flow.t list -> Place.t list -> t
val add_place : Place.t -> t -> unit
val add_event : Event.t -> t -> unit
val add_places : Set.Make(Place).t -> t -> unit
val add_events : Set.Make(Event).t -> t -> unit
val add_arc : Node.t -> Node.t -> t -> unit
val set_marking : Set.Make(Place).t -> t -> unit
val inputs_of : Node.t -> t -> Set.Make(Node).t
val outputs_of : Node.t -> t -> Set.Make(Node).t
val inputs_of_place : Place.t -> t -> Set.Make(Event).t
val outputs_of_place : Place.t -> t -> Set.Make(Event).t
val inputs_of_event : Event.t -> t -> Set.Make(Place).t
val outputs_of_event : Event.t -> t -> Set.Make(Place).t
val enables : Set.Make(Place).t -> Place.t -> t -> bool
val fire : Event.t -> t -> unit
val fire_sequence : Event.t list -> t -> unit
val list_of_marking : t -> Place.t list
val is_occurrence_sequence : Event.t list -> t -> bool
val predecessors : Node.t -> t -> Set.Make(Node).t
val is_predecessor : Node.t -> Node.t -> t -> bool
val is_causally_related : Node.t -> Node.t -> t -> bool
val conflicts : Node.t -> Node.t -> t -> bool
val is_concurrent : Node.t -> Node.t -> t -> bool
