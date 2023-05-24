module PNet = Petrinet.Make(State)(Product_transition)
open PNet

module StateSet = Set.Make(State)
module TransSet = Set.Make(Product_transition)
module PFlowSet = Set.Make(Flow)


(* Product of a list of nets ns given a synchronization constraint sync,
   represented as a list of list of transition options. The i-th element of
   an global transition of sync corresponds to some transition of the i-th 
   component or None if that component doesn't participate.
   Components must not share names of states and transitions. 
   Component order must match the order of local transitions. *)
let product (ns : t list) (sync : (Product_transition.t option list) list) =
  let open Flow in

  let rec trans_of_list = function
      [] -> ""
    | [None] -> Product_transition.idle
    | [Some t] -> t
    | None::ts -> Product_transition.idle ^ Product_transition.sep ^ (trans_of_list ts)
    | Some t::ts -> t ^ Product_transition.sep ^ (trans_of_list ts)
  in

  let parse_preflow trans =
    let preset =
      List.fold_left2
        (fun acc n -> function
            None -> acc
          | Some t -> StateSet.union (inputs_of_trans t n) acc)
        StateSet.empty
        ns
        trans

    in StateSet.fold
      (fun s acc -> PFlowSet.add (s @--> trans_of_list trans) acc)
      preset
      PFlowSet.empty
  in

  let parse_postflow trans =
    let postset =
      List.fold_left2
        (fun acc n -> function
            None -> acc
          | Some t -> StateSet.union (outputs_of_trans t n) acc)
        StateSet.empty
        ns
        trans

    in StateSet.fold
      (fun s acc -> PFlowSet.add (trans_of_list trans -->@ s) acc)
      postset
      PFlowSet.empty


  in of_sets
    (List.fold_left
      (fun acc n -> StateSet.union (places n) acc)
      StateSet.empty
      ns)

    (List.fold_left
      (fun acc trans -> TransSet.add (trans_of_list trans) acc)
      TransSet.empty
      sync)

    (List.fold_left
      (fun acc trans -> PFlowSet.union
        (PFlowSet.union (parse_preflow trans) (parse_postflow trans)) acc)
      PFlowSet.empty
      sync)

    (List.fold_left
      (fun acc n -> StateSet.union (marking n) acc)
      StateSet.empty
      ns)
      