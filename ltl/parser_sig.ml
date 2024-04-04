module type S = sig
  module AP : Set.OrderedType
  module LTL : module type of Ltl.Make (AP)

  val inject : string -> AP.t
end
