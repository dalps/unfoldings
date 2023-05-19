type t

val build : int -> string -> t
val name : t -> int
val label : t -> string
val compare : t -> t -> int