module Make (State : Set.OrderedType) (Alpha : Set.OrderedType) = struct
  module StateSet = Set.Make (State) (* PowerFormulaSet *)
  module AlphaSet = Set.Make (Alpha)
  module PowerStateSet = Set.Make (StateSet)

  module Node = struct
    type t = State.t

    let compare = compare
    let hash = Hashtbl.hash
    let equal = ( = )
  end

  module Edge = struct
    type t = [ `AP of Alpha.t | `Def ]

    let compare = compare
    let equal = ( = )
    let default = `Def
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

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

  let unbind_state f q a r q' a' =
    if q' = q && a' = a then StateSet.remove r (f q a) else f q' a'

  let unbind_state_tot f q q' a' = if q' = q then StateSet.empty else f q' a'
  let bind_f f f' q a = StateSet.union (f q a) (f' q a)

  let remove_state_func u r =
    StateSet.fold
      (fun q acc1 ->
        bind_f
          (AlphaSet.fold (fun a acc2 -> unbind_state acc2 q a r) u.alpha acc1)
          acc1)
      u.states u.func

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

  let remove_state u s =
    of_sets
      (StateSet.remove s u.states)
      u.alpha
      (unbind_state_tot u.func s)
      (StateSet.remove s u.init)
      (let fin1 = PowerStateSet.map (fun b -> StateSet.remove s b) u.fin in
       PowerStateSet.filter (fun f -> not (StateSet.is_empty f)) fin1)

  let get_graph gnba =
    let g = G.create () in
    StateSet.iter
      (fun s ->
        G.add_vertex g s;
        AlphaSet.iter
          (fun a ->
            StateSet.iter
              (fun r -> G.add_edge_e g (s, `AP a, r))
              (gnba.func s a))
          gnba.alpha)
      gnba.states;
    g

  open Plotlib
  module Plotter = Plotter.Make (G)

  let get_style n (module CustomStyle : Plotter.Style) =
    Plotter.extend_style
      (module CustomStyle)
      (module struct
        include Plotter.DefaultStyle

        let graph_attributes _ = [ `Overlap false ]

        let edge_label ((_, x, _) as e) =
          match x with
          | `AP _ -> edge_label e
          | `Def -> ""
        let edge_attributes _ = [ `Dir `Forward ]

        let vertex_attributes v =
          [ `Shape `Ellipse ]
          @ (if PowerStateSet.exists (fun accset -> StateSet.mem v accset) n.fin
             then [ `Peripheries 2 ]
             else [])
          @
          if StateSet.mem v n.init then
            [ `Style `Filled; `ColorWithTransparency 0x00000044l ]
          else []
      end)

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
