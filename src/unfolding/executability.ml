module Make (Net : Petrilib.Petrinet.S) = struct
  module Unfolder = Unfold.Make (Net)
  open Unfolder
  open OccurrenceNet

  module ExecutabilitySS : SearchScheme = struct
    let is_terminal (e : Event.t) n stgy _ =
      TransSet.exists
        (fun e' ->
          stgy (Event.history e') (Event.history e) < 0
          && PTNet.PlaceSet.equal
               (places_of_tokens (postset_t n e'))
               (places_of_tokens (postset_t n e)))
        (transitions n)

    let is_successful e _ _ goals = List.mem (Event.label e) goals
  end

  include Tester (ExecutabilitySS)
end
