module type S = sig
  module PTNet : Petrinet.S
  module OccurrenceNet : Occurrence_net.S

  val tokens_of_places :
    OccurrenceNet.place1 list -> PTNet.trans list -> OccurrenceNet.PlaceSet.t

  val places_of_tokens : OccurrenceNet.PlaceSet.t -> PTNet.PlaceSet.t
  val unfold_init : PTNet.t -> OccurrenceNet.t

  module UnfoldResult : sig
    type t = { event : OccurrenceNet.trans; prefix : OccurrenceNet.t }

    val compare : 'a -> 'a -> int
  end

  val unfold_1 : OccurrenceNet.t -> int -> PTNet.t -> UnfoldResult.t list

  type strategy = PTNet.trans list -> PTNet.trans list -> int

  module type SearchScheme = sig
    val is_terminal :
      OccurrenceNet.trans ->
      OccurrenceNet.t ->
      strategy ->
      PTNet.trans list ->
      bool

    val is_successful :
      OccurrenceNet.trans ->
      OccurrenceNet.t ->
      strategy ->
      PTNet.trans list ->
      bool
  end

  module Tester : functor (_ : SearchScheme) -> sig
    val test : PTNet.t -> strategy -> PTNet.trans list -> int -> bool
  end
end

module Make (Net : Petrinet.S) :
  S
    with module PTNet = Net
     and module OccurrenceNet = Occurrence_net.Make(Net.Place)(Net.Trans)
