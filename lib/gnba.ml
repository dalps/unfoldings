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
    type t = AP of Alpha.t | Def

    let compare = compare
    let equal = ( = )
    let default = Def
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

  let get_graph gnba =
    let g = G.create () in
    StateSet.iter
      (fun s ->
        AlphaSet.iter
          (fun a ->
            StateSet.iter (fun r -> G.add_edge_e g (s, AP a, r)) (gnba.func s a))
          gnba.alpha)
      gnba.states;
    g

  let print_graph n ?(vertex_name = fun v -> string_of_int (G.V.hash v))
      ?(edge_label = fun _ -> "") ?(file_name = "gnba") () =
    let module Plotter = Graph.Graphviz.Neato (struct
      include G

      let graph_attributes _ = [ `Overlap false ]

      let edge_attributes (_, e, _) =
        [
          `Label (match e with Edge.AP ap -> edge_label ap | Def -> "");
          `Dir `Forward;
        ]

      let default_edge_attributes _ = []
      let get_subgraph _ = None

      let vertex_attributes v =
        (if PowerStateSet.exists (fun accset -> StateSet.mem v accset) n.fin
         then [ `Shape `Ellipse; `Peripheries 2 ]
         else [ `Shape `Ellipse ])
        @
        if StateSet.mem v n.init then
          [ `Style `Filled; `ColorWithTransparency 0x00000044l ]
        else []

      let vertex_name v = "\"" ^ vertex_name v ^ "\""
      let default_vertex_attributes _ = []
    end) in
    let g = get_graph n in
    let file = open_out_bin (file_name ^ ".dot") in
    Plotter.output_graph file g;
    Sys.command ("neato -Tpng " ^ file_name ^ ".dot -o " ^ file_name ^ ".png")

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
