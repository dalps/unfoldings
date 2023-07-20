module Make : functor (Net : Petrinet.S) -> sig
  type local_t = Idle | T of Net.Trans.t

  module GlobalTransition : sig
    type t = local_t list

    val is_idle : local_t -> bool
    val participates : int -> t -> bool
    val projection : int -> t list -> t list
    val is_independent : t -> t -> bool
    val compare : t -> t -> int
  end

  include module type of Petrinet.Make (Net.Place) (GlobalTransition)

  val product : Net.t list -> GlobalTransition.t list -> t
end
