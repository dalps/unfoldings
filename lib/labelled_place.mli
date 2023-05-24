type t = {
  name : int;
  label : State.t
}

val build : int -> State.t -> t
val name : t -> int
val label : t -> State.t
val compare : t -> t -> int