module BPNet = Petrinet.Make(State)(Event)
open BPNet

module StateSet = Set.Make(State)
module EventSet = Set.Make(Event)
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