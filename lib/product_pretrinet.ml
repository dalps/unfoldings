module PNet = Petrinet.Make(State)(Product_transition)
open PNet

module StateSet = Set.Make(State)
module TransSet = Set.Make(Product_transition)
module PFlowSet = Set.Make(Flow)


(* Product of two nets given a synchronization constraint **on the transs**.
  The synchronization constraint is represented as a list of pairs of trans
  options. The first (second) component of a pair is an trans of the lhs (rhs)
  operand or None if that operand doesn't participate in the new transition.
  *)
let product (n1 : t) (n2 : t) (sync : (Product_transition.t option * Product_transition.t option) list) =
  let open Flow in

  let trans_of_pair = function
    | None, None -> raise Product_transition.IllegalGlobalTransition
    | Some e, None -> e ^ Product_transition.sep ^ Product_transition.idle
    | None, Some e -> Product_transition.idle ^ Product_transition.sep ^ e
    | Some e1', Some e2' -> e1' ^ Product_transition.sep ^ e2'
  in

  let parse_preflow epair =
    (* Gather all input places of the global transition *)
    let preset = match epair with
      | None, None -> raise Product_transition.IllegalGlobalTransition
      | Some e, None -> inputs_of_trans e n1
      | None, Some e -> inputs_of_trans e n2
      | Some e1, Some e2 -> 
          StateSet.union (inputs_of_trans e1 n1) (inputs_of_trans e2 n2)

    in StateSet.fold
      (fun p acc -> PFlowSet.add (p @--> trans_of_pair epair) acc)
      preset
      PFlowSet.empty
  in

  let parse_postflow epair =
    (* Gather all output places of the global transition *)
    let postset = match epair with
      | None, None -> raise Product_transition.IllegalGlobalTransition
      | Some e, None -> outputs_of_trans e n1
      | None, Some e -> outputs_of_trans e n2
      | Some e1, Some e2 -> 
          StateSet.union (outputs_of_trans e1 n1) (outputs_of_trans e2 n2)

    in StateSet.fold
      (fun p acc -> PFlowSet.add (trans_of_pair epair -->@ p) acc)
      postset
      PFlowSet.empty
  in

  of_sets
    (StateSet.union (places n1) (places n2))

    (List.fold_left
      (fun eset epair -> TransSet.add (trans_of_pair epair) eset)
      TransSet.empty
      sync)
      
    (List.fold_left
      (fun fset epair -> PFlowSet.union
          (PFlowSet.union (parse_preflow epair) (parse_postflow epair)) fset)
      PFlowSet.empty
      sync)

    (StateSet.union (marking n1) (marking n2))