module Make (State : Set.OrderedType) (Alpha : Set.OrderedType) = struct
  module StateSet = Set.Make (State) (* PowerFormulaSet *)
  module AlphaSet = Set.Make (Alpha)
  module PowerStateSet = Set.Make (StateSet)

  type t = {
    states : StateSet.t; (* a set of sets of formulae *)
    alpha : AlphaSet.t;
    func : State.t -> Alpha.t -> StateSet.t;
    init : StateSet.t;
    fin : PowerStateSet.t; (* a set of sets of sets of formulae *)
  }

  let bottom _ _ = StateSet.empty

  let bind_states f q a post q' a' =
    if q' = q && a' = a then StateSet.union post (f q a) else f q' a'

  let bind_state f q a r q' a' =
    if q' = q && a' = a then StateSet.add r (f q a) else f q' a'

  let bind_f f f' q a = StateSet.union (f q a) (f' q a)
  let delta q a post = bind_states bottom q a (StateSet.of_list post)
  let of_sets states alpha func init fin = { states; alpha; func; init; fin }

  let of_lists states alpha func init fin =
    {
      states = StateSet.of_list states;
      alpha = AlphaSet.of_list alpha;
      func = List.fold_right bind_f func bottom;
      init = StateSet.of_list init;
      fin = PowerStateSet.of_list (List.map StateSet.of_list fin);
    }

  module NumberedState = struct
    type t = State.t * int

    let compare = compare
  end

  module NumberedNba = Nba.Make (NumberedState) (Alpha)

  let to_nba gnba =
    let k = PowerStateSet.cardinal gnba.fin in
    let rec range k = if k = 0 then [] else range (k - 1) @ [ k ] in
    let numbered_fin =
      List.combine (range k) (PowerStateSet.elements gnba.fin)
    in

    if k >= 1 then
      NumberedNba.of_sets
        (List.fold_left
           (fun acc i ->
             StateSet.fold
               (fun q acc' -> NumberedNba.StateSet.add (q, i) acc')
               gnba.states acc)
           NumberedNba.StateSet.empty (range k))
        gnba.alpha
        (fun (q, i) a ->
          if not (StateSet.mem q (List.assoc i numbered_fin)) then
            StateSet.fold
              (fun q' -> NumberedNba.StateSet.add (q', i))
              (gnba.func q a) NumberedNba.StateSet.empty
          else
            StateSet.fold
              (fun q' -> NumberedNba.StateSet.add (q', (i mod k) + 1))
              (gnba.func q a) NumberedNba.StateSet.empty)
        (StateSet.fold
           (fun q0 -> NumberedNba.StateSet.add (q0, 1))
           gnba.init NumberedNba.StateSet.empty)
        (StateSet.fold
           (fun qf -> NumberedNba.StateSet.add (qf, 1))
           (List.assoc 1 numbered_fin)
           NumberedNba.StateSet.empty
           (* If k = 0 every infinite run needs to be accepted! *))
    else
      let states =
        StateSet.fold
          (fun q acc' -> NumberedNba.StateSet.add (q, 0) acc')
          gnba.states NumberedNba.StateSet.empty
      in
      NumberedNba.of_sets states gnba.alpha
        (fun (q, _) a ->
          StateSet.fold
            (fun q' -> NumberedNba.StateSet.add (q', 0))
            (gnba.func q a) NumberedNba.StateSet.empty)
        (StateSet.fold
           (fun q0 -> NumberedNba.StateSet.add (q0, 0))
           gnba.init NumberedNba.StateSet.empty)
        states
end
