module type S = sig
  module N : Petrinet.S
  module P : module type of Product.Make (N)

  val inject_place : string -> N.place
  val inject_trans : string -> N.trans
end
