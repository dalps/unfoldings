type t = {
  history : Product_transition.t list;
  label : State.t
}

val build : Product_transition.t list -> State.t -> t
val history : t -> Product_transition.t list
val label : t -> State.t
val compare : t -> t -> int