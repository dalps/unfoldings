module LabelledPlace = struct
  type t = {
    history : Product.Trans.t list;
    label : State.t
  }

  let build history label = {history; label}

  let history p = p.history

  let label p = p.label

  let compare (p1 : t) (p2 : t) = 
    let n = History_utils.sl_compare p1.history p2.history in
    if n = 0 then compare p1.label p2.label
    else n
end

module Event = struct
  type t = {
    name : int;
    history : Product.Trans.t list;
    label : Product.Trans.t
  }

  let build name history label = {name; history; label}

  let name e = e.name

  let history e = e.history

  let label e = e.label
    
  let compare e1 e2 =
    let n = History_utils.sl_compare e1.history e2.history in
    if n = 0 then compare e1.label e2.label
    else n
end

include Petrinet.Make(LabelledPlace)(Event)

let predecessors x n =
  let rec helper p parents =
    (NodeSet.fold 
      (fun parent acc -> 
        if NodeSet.mem parent acc then acc 
        else helper parent (NodeSet.add parent acc))
      (preset_x n p)
      parents)

  in helper x NodeSet.empty

let parents_of_event e n =
  PlaceSet.fold
    (fun p acc -> TransSet.union (preset_p n p) acc)
    (preset_t n e)
    TransSet.empty

let past e n =
  let rec helper p word =
    let parents_of_p = parents_of_event p n in (* get the immediate predecessors *)
    (* add each parent IF NOT already present in the result *)
    (TransSet.fold 
      (fun parent acc -> 
        if List.mem parent acc then acc 
        else helper parent ((TransSet.elements parents_of_p) @ acc))
      parents_of_p
      word)

  in (helper e []) @ [e] (* e itself is part of its past *)

let past_word e n = List.map Event.label (past e n)

let past_conf e n = TransSet.add e (NodeSet.fold
  (fun x acc -> TransSet.add (Node.trans_of x) acc)
  (NodeSet.filter Node.is_trans (predecessors (Node.of_trans e) n))
  TransSet.empty)

let past_of_preset ps n =
  let parent_events = PlaceSet.fold
    (fun p acc -> 
      let input_event = preset_p n p in
      assert (TransSet.cardinal input_event <= 1);
      TransSet.union input_event acc)
    ps
    TransSet.empty
  in

  let rec helper (ps : TransSet.t) word =
    (TransSet.fold
      (fun p acc -> 
        let parents_of_p = parents_of_event p n in
        (TransSet.fold
          (fun parent acc' -> 
            if List.mem parent acc' then acc' 
            else helper parents_of_p ((TransSet.elements parents_of_p) @ acc'))
          parents_of_p
          acc))
      ps
      word)

  in helper parent_events (TransSet.elements parent_events)

let past_word_of_preset ps n t = 
  List.map Event.label (past_of_preset ps n) @ [t]
        
let place_history p n = match TransSet.elements (preset_p n p) with
  | [] -> []
  | e::_ -> past_word e n

let is_predecessor x y n = NodeSet.mem x (predecessors y n)

let is_causally_related x y n = is_predecessor x y n || is_predecessor y x n

(* conflicts x y n checks whether nodes x and y are in conflict in net n,
    i.e. they cannot be fired in the same execuiton (in case of events).
    Holds iff n is acyclic. *)
let is_conflict x y n = 
  let pred_x = predecessors x n in
  let pred_y = predecessors y n in
  (* x and y could be output events of the place that starts the conflict, 
    so consider them too as their own predecessors. *)
  let pred_x_only = NodeSet.add x (NodeSet.diff pred_x pred_y) in
  let pred_y_only = NodeSet.add y (NodeSet.diff pred_y pred_x) in
  let pred_places_in_common = 
    NodeSet.filter (Node.is_place) (NodeSet.inter pred_x pred_y) in

  NodeSet.exists
  (fun p -> let es = postset_x n p in
    NodeSet.fold
    (fun e acc -> acc ||
      if NodeSet.mem e pred_x_only then
        NodeSet.exists (fun e' -> e <> e' && NodeSet.mem e' pred_y_only) es
      else
        if NodeSet.mem e pred_y_only then
          NodeSet.exists (fun e' -> e <> e' && NodeSet.mem e' pred_x_only) es
        else
          false
    )
    es
    false
  )
  pred_places_in_common

let is_concurrent x y n =
  not (is_causally_related x y n) && not (is_conflict x y n)

let is_reachable m n =
  let nodes = nodeset_of_placeset m in
  NodeSet.for_all
    (fun x -> NodeSet.for_all (fun y -> is_concurrent x y n) nodes)
    nodes

let union bp1 bp2 = of_sets
  (PlaceSet.union (places bp1) (places bp2))
  (TransSet.union (transitions bp1) (transitions bp2))
  (bind_f (preset_t bp1) (preset_t bp2))
  (bind_f (postset_t bp1) (postset_t bp2))
  (PlaceSet.union (marking bp1) (marking bp2))
  