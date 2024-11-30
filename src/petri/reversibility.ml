(* This module provides casual-consistent reversible semantics
   for occurrence nets *)

module Make (I : Occurrence_net.S) = struct
  module ReversibleEvent = struct
    type t = [ `E of I.trans | `Rev of t ]

    let rec event_of_t = function
      | `E e -> e
      | `Rev t -> event_of_t t
    let build name history label = `E (I.Event.build name history label)

    let name e' =
      let e = event_of_t e' in
      I.Event.name e

    let history e' =
      let e = event_of_t e' in
      I.Event.history e

    let label e' =
      let e = event_of_t e' in
      I.Event.label e

    let compare_event e1 e2 =
      let n = compare (I.Event.history e1) (I.Event.history e2) in
      if n = 0 then
        compare (I.Event.label e1) (I.Event.label e2)
      else
        n

    let rec compare e1' e2' =
      match (e1', e2') with
      | `E e1, `E e2 -> compare_event e1 e2
      | c, `Rev (`Rev d) | `Rev (`Rev c), d | `Rev c, `Rev d -> compare c d
      | `E _, `Rev _ -> -1
      | `Rev _, `E _ -> 1
  end

  include Petrinet.Make (I.Token) (ReversibleEvent)

  let parent_preset_t = preset_t
  let parent_postset_t = postset_t

  let rec preset_t n = function
    | `E _ as e -> parent_preset_t n e
    | `Rev (`Rev e') -> preset_t n e'
    | `Rev e' -> postset_t n e'

  and postset_t n = function
    | `E _ as e -> parent_postset_t n e
    | `Rev (`Rev e') -> postset_t n e'
    | `Rev e' -> preset_t n e'

  let reverse e n = add_edges (postset_t n e, `Rev e, preset_t n e) n

  let reversible (n : I.t) =
    let ps = I.PlaceSet.lift (module PlaceSet) in
    let ts =
      I.TransSet.fold
        (fun e -> TransSet.add (`E e))
        (I.transitions n) TransSet.empty
    in
    let delta f = function
      | `E e -> f n e |> ps
      | _ -> PlaceSet.empty
    in
    let n' =
      of_sets
        (I.places n |> ps)
        ts (delta I.preset_t) (delta I.postset_t)
        (I.marking n |> ps)
    in
    TransSet.iter (fun e -> reverse e n') (transitions n');
    n'
end
