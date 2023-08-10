module Make (State : Set.OrderedType) (Alpha : Set.OrderedType) = struct
  include Nba.Make (State) (Alpha)
  module PowerStateSet = Set.Make (StateSet)

  type t = {
    states : StateSet.t;
    alpha : AlphaSet.t;
    func : State.t -> Alpha.t -> StateSet.t;
    init : StateSet.t;
    fin : PowerStateSet.t;
  }

  let of_lists states alpha func init fin =
    {
      states = StateSet.of_list states;
      alpha = AlphaSet.of_list alpha;
      func = List.fold_right bind_f func bottom;
      init = StateSet.of_list init;
      fin = PowerStateSet.of_list (List.map StateSet.of_list fin);
    }

  module NumberedNba =
    Nba.Make
      (struct
        type t = State.t * int

        let compare = compare
      end)
      (Alpha)

  let to_nba gnba =
    let k = PowerStateSet.cardinal gnba.fin in
    let rec range k = if k = 0 then [] else range (k - 1) @ [k] in
    let numbered_fin =
      List.combine (range k) (PowerStateSet.elements gnba.fin)
    in

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
         NumberedNba.StateSet.empty)
end
