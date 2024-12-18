open Utils

module Make (State : Set.OrderedType) (Alpha : Set.OrderedType) = struct
  module StateSet = SetUtils.Make (State)
  module AlphaSet = SetUtils.Make (Alpha)

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
    states : StateSet.t;
    alpha : AlphaSet.t;
    func : State.t -> Alpha.t -> StateSet.t;
    init : StateSet.t;
    fin : StateSet.t;
  }

  type trans = { src : State.t; t : Alpha.t; tgt : State.t; id : int }

  let enum_transitions nba =
    AlphaSet.fold
      (fun a ->
        let trans_a =
          StateSet.fold
            (fun b ->
              StateSet.fold (fun b' -> List.cons (b, a, b')) (nba.func b a))
            nba.states []
        in
        ( @ ) (List.mapi (fun id (src, t, tgt) -> { src; t; tgt; id }) trans_a))
      nba.alpha []

  let bottom _ _ = StateSet.empty

  let bind_states f q a post q' a' =
    if q' = q && a' = a then
      StateSet.union post (f q a)
    else
      f q' a'

  let bind_state f q a r q' a' =
    if q' = q && a' = a then
      StateSet.add r (f q a)
    else
      f q' a'

  let bind_f f f' q a = StateSet.union (f q a) (f' q a)
  let delta q a post = bind_states bottom q a (StateSet.of_list post)

  let of_lists states alpha func init fin =
    {
      states = StateSet.of_list states;
      alpha = AlphaSet.of_list alpha;
      func = List.fold_right bind_f func bottom;
      init = StateSet.of_list init;
      fin = StateSet.of_list fin;
    }

  let of_sets states alpha func init fin = { states; alpha; func; init; fin }

  let get_graph gnba =
    let g = G.create () in
    StateSet.iter
      (fun s ->
        AlphaSet.iter
          (fun a ->
            StateSet.iter
              (fun r -> G.add_edge_e g (s, `AP a, r))
              (gnba.func s a))
          gnba.alpha)
      gnba.states;
    g

  let enum_transitions_g nba =
    let g = get_graph nba in
    List.mapi (fun i (_, a, _) -> (a, i)) (G.fold_edges_e List.cons g [])

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
          if StateSet.mem v n.fin then
            [ `Shape `Doublecircle ]
          else
            [ `Shape `Circle ]
      end)
end
