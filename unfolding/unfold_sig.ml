open Petrilib

module type Make = functor (Net : Petrinet.S) -> sig
  module PTNet :
    Petrinet.S
      with type t = Net.t
       and type place = Net.place
       and type trans = Net.trans

  module OccurrenceNet :
    Occurrence_net.S
      with type place1 = PTNet.place
       and type trans1 = PTNet.trans

  val tokens_of_places :
    int ->
    OccurrenceNet.place1 list ->
    PTNet.trans list ->
    OccurrenceNet.PlaceSet.t

  val places_of_tokens : OccurrenceNet.PlaceSet.t -> PTNet.PlaceSet.t
  val unfold_init : PTNet.t -> OccurrenceNet.t

  module UnfoldResult : sig
    type t = { event : OccurrenceNet.trans; prefix : OccurrenceNet.t }

    val compare : 'a -> 'a -> int
  end

  val unfold : int -> PTNet.t -> OccurrenceNet.t

  type strategy = PTNet.trans list -> PTNet.trans list -> int

  module type SearchScheme = sig
    (* assumption: e is feasible *)
    val is_terminal :
      OccurrenceNet.Event.t ->
      OccurrenceNet.t ->
      strategy ->
      PTNet.trans list ->
      bool

    (* assumption: e is feasible *)
    val is_successful :
      OccurrenceNet.Event.t ->
      OccurrenceNet.t ->
      strategy ->
      PTNet.trans list ->
      bool
  end

  module TestResult : sig
    type t = {
      res : bool;
      prefix : OccurrenceNet.t;
      history : PTNet.Trans.t list;
      terms : OccurrenceNet.TransSet.t;
    }
  end

  module Tester : functor (_ : SearchScheme) -> sig
    val test : PTNet.t -> strategy -> PTNet.trans list -> int -> TestResult.t
  end
end
