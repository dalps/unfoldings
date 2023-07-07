type local_t = Idle | T of string

module GlobalTransition : sig
  type t = local_t list

  val is_idle : local_t -> bool
  val participates : int -> t -> bool
  val projection : int -> t list -> t list
  val is_independent : t -> t -> bool
  val compare : t -> t -> int
end

include module type of Petrinet.Make (String) (GlobalTransition)

val product : t list -> GlobalTransition.t list -> t
