module BPNet = Petrinet.Make(Labelled_place)(Event)
open BPNet

module PlaceSet = Set.Make(Labelled_place)
module EventSet = Set.Make(Event)
module BPFlowSet = Set.Make(Flow)
module NodeSet = Set.Make(Node)
  
let predecessors x n =
  let rec helper p parents =
    let inputs_of_p = inputs_of p n in (* get the immediate predecessors *)
    (* add each parent IF NOT already present in the accumulator *)
    (NodeSet.fold 
      (fun parent acc -> 
        if NodeSet.mem parent acc then acc 
        else helper parent (NodeSet.add parent acc))
      inputs_of_p
      parents)

  in helper x NodeSet.empty

  let parents_of_event e n = (* can be generalized to Petrinets *)
  (* always comprises independent events, because an input place may
     only have no more than one causing events (hence they cannot be shared) *)
  let inputs_of_e = inputs_of_trans e n in
  PlaceSet.fold
    (fun p acc -> EventSet.union (inputs_of_place p n) acc)
    inputs_of_e
    EventSet.empty

let past e n =
  let rec helper p word =
    let parents_of_p = parents_of_event p n in (* get the immediate predecessors *)
    (* add each parent IF NOT already present in the result *)
    (EventSet.fold 
      (fun parent acc -> 
        if List.mem parent acc then acc 
        else helper parent ((EventSet.elements parents_of_p) @ acc))
      parents_of_p
      word)

  in (helper e []) @ [e] (* e itself is part of its past *)

let past_word e n = List.map Event.label (past e n)

let past_conf e n = EventSet.add e (NodeSet.fold
  (fun x acc -> EventSet.add (Node.trans_of x) acc)
  (NodeSet.filter Node.is_trans (predecessors (Node.of_trans e) n))
  EventSet.empty)

let input_of_place p n =
  (* by construction, a place may have at most one input event *)
  let e = inputs_of_place p n in

  match EventSet.elements e with
      [] -> None
    | e::_ -> Some e
        
let place_history p n = match input_of_place p n with
    None -> []
  | Some e -> past_word e n

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

  (* Pseudocode:
  
    fun C(n) := predecessors of n

    CX := (C(x)∖C(y)) ⋃ {x} // predecessors of x only
    CY := (C(y)∖C(x)) ⋃ {y} // predecessors of y only
    CP := {p | p ∈ C(x)⋂C(y) and IsState(p)} // common predecessor places

    is_conflict := false
    
    // ∃p ∈ CP . ∃e,e' ∈ Postset(p) . e ≠ e' and e ∈ CX and e' ∈ CY
    for each node p in pred_places_in_common {
      es := outputs_of p

      for each node e in es
        if (e ∈ CX) then
          for each node e' in es
            if (e <> e' and e' ∈ CY)
            then {
              is_conflict := true
              /* break */
            }
        else
          if (e ∈ CY) then
            for each node e' in es
              if (e <> e' and e' ∈ CX)
              then {
                is_conflict := true
                /* break */
              }
    }
  *)
  NodeSet.exists
  (fun p -> let es = outputs_of p n in
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


module NodePair = struct
  type t = Node.t * Node.t
  let compare = compare
end

module NodePairSet = Set.Make(NodePair)

let concurrencies n =
  let nodes = 
    NodeSet.union (nodes_of_places (places n)) (nodes_of_transs (transitions n))
  in

  (* Pseudocode:

    map := NodeMap.empty()

    for each node x in nodes
      for each node y in nodes
        if x co y then map.add x y
  *)
  NodeSet.fold
  (fun x map ->
    (NodeSet.fold
    (fun y map_x -> 
      if is_concurrent x y n 
      then NodePairSet.add (x,y) map_x else map_x)
    nodes
    map))
  nodes
  NodePairSet.empty

let is_reachable m n =
  let co = concurrencies n in
  let nodes = nodes_of_places m in

  (* Pseudocode:
    
    is_reachable := true
    
    for each node x in nodes
      for each node y in nodes
        if (x,y) is not in co_of_nodes then is_reachable := false
        (break)   
  *)
  NodeSet.fold
  (fun x b -> b &&
    NodeSet.fold
    (fun y c -> c && NodePairSet.mem (x,y) co)
    nodes
    b)
  nodes
  true

let print_placeset ps =
  print_string "[";
  List.iter (fun (p : Labelled_place.t) -> print_string "{history = ["; 
    List.iter (fun t -> print_string ("\"" ^ t ^ "\"")) p.history;
    print_string ("]; label = \"" ^ p.label ^ "\"}; ")) 
  (PlaceSet.elements ps);
  print_endline "]\n"

let union bp1 bp2 = BPNet.of_sets
  (PlaceSet.union (BPNet.places bp1) (BPNet.places bp2))
  (EventSet.union (BPNet.transitions bp1) (BPNet.transitions bp2))
  (BPFlowSet.union (BPNet.flow bp1) (BPNet.flow bp2))
  (PlaceSet.union (BPNet.marking bp1) (BPNet.marking bp2))
  