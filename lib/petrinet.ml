type place = string
type event = string

module Place = struct
  type t = place

  let compare = compare
end

module Event = struct
  type t = event

  let compare = compare
end

type node = P of Place.t | E of Event.t

module Node = struct
  type t = node

  let compare = compare
  let is_place = function P _ -> true | E _ -> false
  let is_event = function P _ -> false | E _ -> true
end

module Flow = struct
  type t = {source: node; label: string; target: node} 

  let compare = compare
end

module PlaceSet = Set.Make(Place)
module EventSet = Set.Make(Event)
module FlowSet = Set.Make(Flow)
module NodeSet = Set.Make(Node)

module PetriNet = struct
  type t = {
    mutable places: PlaceSet.t;
    mutable events: EventSet.t;
    mutable flow: FlowSet.t;
    mutable marking: PlaceSet.t
  }

  let empty = {
    places = PlaceSet.empty;
    events = EventSet.empty;
    flow = FlowSet.empty;
    marking = PlaceSet.empty
  }

  exception IllegalFlow

  let add_place p n = n.places <- PlaceSet.add p n.places

  let add_event e n = n.events <- EventSet.add e n.events

  let add_places ps n = n.places <- PlaceSet.union ps n.places

  let add_events es n = n.events <- EventSet.union es n.events

  let add_arc src_node lbl tgt_node n = n.flow <-
    if (match src_node,tgt_node with
      P p, E e | E e, P p -> PlaceSet.mem p n.places && EventSet.mem e n.events
    | _ -> false)
    then
      FlowSet.add {source = src_node; label = lbl; target = tgt_node} n.flow
    else
      raise IllegalFlow
        
  let init_marking m n = n.marking <- if PlaceSet.subset m n.places then m else n.marking

  let inputs_of x n = FlowSet.fold 
    (fun f acc -> NodeSet.add f.source acc) 
    (FlowSet.filter (fun f -> f.target = x) n.flow)
    NodeSet.empty

  let outputs_of x n = FlowSet.fold 
    (fun f acc -> NodeSet.add f.target acc) 
    (FlowSet.filter (fun f -> f.source = x) n.flow)
    NodeSet.empty

  let inputs_of_place p n = 
    let flows = FlowSet.filter (fun f -> f.target = P p) n.flow in
    FlowSet.fold 
      (fun f acc -> EventSet.add (match f.source with P _ -> raise IllegalFlow | E e -> e) acc) 
      flows
      EventSet.empty

  let outputs_of_place p n = 
    let flows = FlowSet.filter (fun f -> f.source = P p) n.flow in
    FlowSet.fold 
      (fun f acc -> EventSet.add (match f.target with P _ -> raise IllegalFlow | E e -> e) acc) 
      flows
      EventSet.empty

  let inputs_of_event e n =
    let flows = FlowSet.filter (fun f -> f.target = E e) n.flow in
    FlowSet.fold 
      (fun f acc -> PlaceSet.add (match f.source with E _ -> raise IllegalFlow | P p -> p) acc) 
      flows
      PlaceSet.empty

  let outputs_of_event e n =
    let flows = FlowSet.filter (fun f -> f.source = E e) n.flow in
    FlowSet.fold 
      (fun f acc -> PlaceSet.add (match f.target with E _ -> raise IllegalFlow | P p -> p) acc) 
      flows
      PlaceSet.empty

  let enables m e n = PlaceSet.subset (inputs_of_event e n) m

  (* If the current marking of n enables e, fires e and updates n's marking *)
  let fire e n = if enables n.marking e n then
    let input = inputs_of_event e n in
    let output = outputs_of_event e n in
    n.marking <- PlaceSet.union (PlaceSet.diff n.marking input) output

  (* Fire a sequence of events, ignoring those not enabled *)
  let fire_sequence es n =
    List.fold_left (fun _ e -> fire e n) () es

  let list_of_marking n = PlaceSet.elements n.marking

  let is_occurrence_sequence es n =
    let rec helper elist m = match elist with
      [] -> true
    | e::es' ->
        let input = inputs_of_event e n in
        let output = outputs_of_event e n in
        let m' = PlaceSet.union (PlaceSet.diff m input) output in
        enables m e n && helper es' m'

    in helper es n.marking
    
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

  (* let rec is_predecessor x y n =
    let inputs_of_y = inputs_of y n in
    if 
    NodeSet.fold
      (fun x' acc -> if x = x' then true else is_predecessor x x' n && acc)
      inputs_of_y
      true
  *)

  (* conflicts x y n checks whether nodes x and y are in conflict in net n,
     i.e. they cannot be fired in the same execuiton (in case of events).
     Holds iff n is acyclic. *)
  let conflicts x y n = 
    let pred_x = predecessors x n in
    let pred_y = predecessors y n in
    let pred_x_only = NodeSet.diff pred_x pred_y in
    let pred_y_only = NodeSet.diff pred_y pred_x in
    let pred_places_in_common = 
      NodeSet.filter (Node.is_place) (NodeSet.inter pred_x pred_y) in

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
    not (is_causally_related x y n) && not (conflicts x y n) 
    
end