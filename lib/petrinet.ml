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

module Flow = struct
  type t = {source: node; label: string; target: node} 

  let compare = compare
end

module PlaceSet = Set.Make(Place)
module EventSet = Set.Make(Event)
module FlowSet = Set.Make(Flow)

module NodeSets = struct
  type t = PS of PlaceSet.t | ES of EventSet.t

  let compare = compare
end

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

  let inputs_of x n = 
    let flows = FlowSet.filter (fun f -> f.target = x) n.flow in
    match x with
      P _ -> NodeSets.ES (FlowSet.fold 
      (fun f acc -> EventSet.add (match f.source with P _ -> raise IllegalFlow | E e -> e) acc) 
      flows
      EventSet.empty)
    | E _ -> NodeSets.PS (FlowSet.fold 
      (fun f acc -> PlaceSet.add (match f.source with E _ -> raise IllegalFlow | P p -> p) acc) 
      flows
      PlaceSet.empty)

    let outputs_of x n = 
      let flows = FlowSet.filter (fun f -> f.source = x) n.flow in
      match x with
        P _ -> NodeSets.ES (FlowSet.fold 
        (fun f acc -> EventSet.add (match f.target with P _ -> raise IllegalFlow | E e -> e) acc) 
        flows
        EventSet.empty)
      | E _ -> NodeSets.PS (FlowSet.fold 
        (fun f acc -> PlaceSet.add (match f.target with E _ -> raise IllegalFlow | P p -> p) acc) 
        flows
        PlaceSet.empty)
    
      
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

  let list_of_marking n = PlaceSet.elements n.marking 
end