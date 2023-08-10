module Make (State : Set.OrderedType) (Alpha : Set.OrderedType) = struct
  module StateSet = Set.Make (State)
  module AlphaSet = Set.Make (Alpha)
  module PowerStateSet = Set.Make (StateSet)

  type t = {
    states : StateSet.t;
    alpha : AlphaSet.t;
    func : State.t -> Alpha.t -> StateSet.t;
    init : StateSet.t;
    fin : PowerStateSet.t;
  }

  let bottom _ _ = StateSet.empty

  let bind_states f q a post q' a' =
    if q' = q && a' = a then StateSet.union post (f q a) else f q' a'

  let bind_state f q a r q' a' =
    if q' = q && a' = a then StateSet.add r (f q a) else f q' a'

  let bind_f f f' q a = StateSet.union (f q a) (f' q a)
  let delta q a post = bind_states bottom q a (StateSet.of_list post)

  let of_lists states alpha func init fin =
    {
      states = StateSet.of_list states;
      alpha = AlphaSet.of_list alpha;
      func = List.fold_right bind_f func bottom;
      init = StateSet.of_list init;
      fin = PowerStateSet.of_list (List.map StateSet.of_list fin);
    }
end
