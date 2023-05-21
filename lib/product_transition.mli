type t = string

exception IllegalGlobalTransition

val sep : t
val idle : t
val represents_local_transition : t -> bool
val is_idle : t -> bool
val explode : t -> t list
val is_well_formed : t -> bool
val participates : int -> t -> bool
val projection : int -> t list -> t list
val is_independent : t -> t -> bool
val tword_equiv : t list -> t list -> bool
val trace : t list -> t list list
val concat_traces : t list -> t list -> t list list
val to_alpha : t -> t
val to_alpha_word : t list -> t
val sl_compare : t list -> t list -> int
val projections : t list -> t list list
val d_compare : (t list -> t list -> int) -> t list -> t list -> int
val compare : t -> t -> int
