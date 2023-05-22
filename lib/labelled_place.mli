type t

val build : int -> Product_transition.t -> t
val name : t -> int
val label : t -> Product_transition.t
val compare : t -> t -> int