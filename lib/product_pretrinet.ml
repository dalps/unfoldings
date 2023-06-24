include Petrinet.Make(State)(Product_transition)

(* Product of a list of nets ns given a synchronization constraint sync,
   represented as a list of list of transition options. The i-th element of
   an global transition of sync corresponds to some transition of the i-th 
   component or None if that component doesn't participate.
   Components must not share names of states and transitions. 
   Component order must match the order of local transitions. *)
let product (ns : t list) (sync : Product_transition.t list) =
  let parse_preflow (trans : Product_transition.t) =
    let preset =
      List.fold_left2
        (fun acc n t -> PlaceSet.union (inputs_of_trans [t] n) acc)
        PlaceSet.empty
        ns
        trans
    in PlaceSet.fold
      (fun s acc -> FlowSet.add (s @--> trans) acc)
      preset
      FlowSet.empty
  in

  let parse_postflow trans =
    let postset =
      List.fold_left2
        (fun acc n t -> PlaceSet.union (outputs_of_trans [t] n) acc)
        PlaceSet.empty
        ns
        trans
    in PlaceSet.fold
      (fun s acc -> FlowSet.add (trans -->@ s) acc)
      postset
      FlowSet.empty

  in of_sets
    (List.fold_left
      (fun acc n -> PlaceSet.union (places n) acc)
      PlaceSet.empty
      ns)
    (TransSet.of_list sync)
    (List.fold_left
      (fun acc trans -> FlowSet.union
        (FlowSet.union (parse_preflow trans) (parse_postflow trans)) acc)
      FlowSet.empty
      sync)
    (List.fold_left
      (fun acc n -> PlaceSet.union (marking n) acc)
      PlaceSet.empty
      ns)
      