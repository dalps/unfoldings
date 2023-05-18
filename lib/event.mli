type t

val build : int -> string -> t
val build_anon : string -> t
val name_of : t -> int
val label_of : t -> string
val compare : t -> t -> int