type t = string

exception IllegalGlobalTransition

val __SEP__ : char
val __IDLE__ : char
val sep : string
val idle : string
val represents_local_transition : string -> bool
val is_idle : string -> bool
val explode : string -> string list
val is_well_formed : string -> bool
val participates : int -> string -> bool
val projection : int -> string list -> string list
val is_independent : string -> string -> bool
val is_equivalent : string list -> string list -> bool
val compare : t -> t -> int
