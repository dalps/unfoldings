type t = {
  name : int;
  history : Product_transition.t list;
  label : Product_transition.t
}

val build : int -> Product_transition.t list -> Product_transition.t -> t
val name : t -> int
val history : t -> Product_transition.t list
val label : t -> Product_transition.t
val compare : t -> t -> int