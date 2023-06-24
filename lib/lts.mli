module Trans :
  sig
    type t
    val step : t -> State.t * t * State.t
    val enables : State.t -> t -> bool
    val compare : 'a -> 'a -> int
  end

val (-->) : State.t -> State.t -> string -> Trans.t
  
type t
val empty : unit -> t
val build : State.t list -> Trans.t list -> State.t -> t
val add_state : State.t -> t -> unit
val add_trans : Trans.t -> t -> unit
val set_init_state : State.t -> t -> unit
val is_computation : string list -> t -> bool
val is_history : string list -> t -> bool
val product_component_of : t -> Product.t
