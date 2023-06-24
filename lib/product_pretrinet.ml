module PNet = Petrinet.Make(State)(Product_transition)

module StateSet = Set.Make(State)
module TransSet = Set.Make(Product_transition)
module PFlowSet = Set.Make(PNet.Flow)

(* Product of a list of nets ns given a synchronization constraint sync,
   represented as a list of list of transition options. The i-th element of
   an global transition of sync corresponds to some transition of the i-th 
   component or None if that component doesn't participate.
   Components must not share names of states and transitions. 
   Component order must match the order of local transitions. *)
let product (ns : PNet.t list) (sync : Product_transition.t list) =
  let open PNet in
  let parse_preflow (trans : Product_transition.t) =
    let preset =
      List.fold_left2
        (fun acc n t -> StateSet.union (inputs_of_trans [t] n) acc)
        StateSet.empty
        ns
        trans
    in StateSet.fold
      (fun s acc -> PFlowSet.add (s @--> trans) acc)
      preset
      PFlowSet.empty
  in

  let parse_postflow trans =
    let postset =
      List.fold_left2
        (fun acc n t -> StateSet.union (outputs_of_trans [t] n) acc)
        StateSet.empty
        ns
        trans
    in StateSet.fold
      (fun s acc -> PFlowSet.add (trans -->@ s) acc)
      postset
      PFlowSet.empty

  in PNet.of_sets
    (List.fold_left
      (fun acc n -> StateSet.union (places n) acc)
      StateSet.empty
      ns)
    (TransSet.of_list sync)
    (List.fold_left
      (fun acc trans -> PFlowSet.union
        (PFlowSet.union (parse_preflow trans) (parse_postflow trans)) acc)
      PFlowSet.empty
      sync)
    (List.fold_left
      (fun acc n -> StateSet.union (marking n) acc)
      StateSet.empty
      ns)
      