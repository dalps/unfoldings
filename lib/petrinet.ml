module Place = struct
  type t = string

  let compare = compare
end

module Event = struct
  type t = string

  exception IllegalGlobalEvent

  let __ID__ = '#' 
  let __SEP__ = ','
  let __IDLE__ = '_'

  let id = Char.escaped __ID__
  let sep = Char.escaped __SEP__
  let idle = Char.escaped __IDLE__

  let has_id e = String.contains e __ID__

  let set_id i e = 
    assert (not (has_id e)); (* Event argument must not be identified already *)
    e ^ id ^ (Int.to_string i)

  let get_id e =
    assert (has_id e);
    let i = String.index e __ID__ in
    int_of_string (String.sub e (i+1) ((String.length e)-i-1))

  let label_of e =
    if has_id e then
      let i = String.index e __ID__ in
      String.sub e 0 i

    else e

  let is_transition e =
    not (has_id e) &&
    not (String.contains e __SEP__) &&
    not (String.contains e __IDLE__)

  let is_idle = (=) idle

  let explode e =
    String.split_on_char __SEP__ (label_of e)

  let participates i e = List.nth (explode e) i <> Char.escaped __IDLE__

  let projection i h = List.filter (participates i) h

  let is_independent e1 e2 = List.for_all
    (fun (le1,le2) -> is_idle le1 <> is_idle le2)
    (List.combine (explode e1) (explode e2))

  let of_pair = function
    | None, None -> raise IllegalGlobalEvent
    | Some e, None -> e ^ sep ^ idle
    | None, Some e -> idle ^ sep ^ e
    | Some e1, Some e2 -> e1 ^ sep ^ e2

  let compare = compare
end

module Node = struct
  type t = P of Place.t | E of Event.t

  let compare = compare
  let is_place = function P _ -> true | E _ -> false
  let is_event = function P _ -> false | E _ -> true
  let of_place p = P p
  let of_event e = E e

  exception NotAPlace
  exception NotAnEvent
  
  let place_of = function P p -> p | _ -> raise NotAPlace
  let event_of = function E e -> e | _ -> raise NotAnEvent
end

module Flow = struct
  type t = {source: Node.t; target: Node.t} 

  exception IllegalFlow

  let compare = compare

  let build src tgt =
    if
      Node.is_place src && Node.is_event tgt ||
      Node.is_event src && Node.is_place tgt
    then
      {source = src; target = tgt}
    else
      raise IllegalFlow

  let source f = f.source
  let target f = f.target
      
  let to_place e p = {source = Node.of_event e; target = Node.of_place p}
  let to_event p e = {source = Node.of_place p; target = Node.of_event e}

  let target_event f = Node.event_of f.target
  let source_event f = Node.event_of f.source
  let target_place f = Node.place_of f.target
  let source_place f = Node.place_of f.source
end

let (@-->) = Flow.to_event
let (-->@) = Flow.to_place

module PlaceSet = Set.Make(Place)
module EventSet = Set.Make(Event)
module FlowSet = Set.Make(Flow)
module NodeSet = Set.Make(Node)

type t = {
  mutable places: PlaceSet.t;
  mutable events: EventSet.t;
  mutable flow: FlowSet.t;
  mutable marking: PlaceSet.t
}

let empty () = {
  places = PlaceSet.empty;
  events = EventSet.empty;
  flow = FlowSet.empty;
  marking = PlaceSet.empty
}

let build ps es fs im =
  assert (List.for_all Event.is_transition es);
  let n = empty () in
  n.places <- PlaceSet.of_list ps;
  n.events <- EventSet.of_list es;
  n.flow <- FlowSet.of_list fs;
  n.marking <- PlaceSet.of_list im;
  n

let add_place p n = n.places <- PlaceSet.add p n.places

let add_event e n = 
  assert (Event.is_transition e);
  n.events <- EventSet.add e n.events

let add_places ps n = n.places <- PlaceSet.union ps n.places

let add_events es n =
  assert (List.for_all Event.is_transition es);
  n.events <- EventSet.union (EventSet.of_list es) n.events

exception UnknownPlace of Place.t
exception UnknownEvent of Event.t

let add_to_place_arc e p n = 
  if PlaceSet.mem p n.places 
  then
    if EventSet.mem e n.events
    then n.flow <- FlowSet.add (Flow.to_place e p) n.flow
    else raise (UnknownEvent e)
  else raise (UnknownPlace p)

let add_to_event_arc p e n = 
  if PlaceSet.mem p n.places 
  then
    if EventSet.mem e n.events
    then n.flow <- FlowSet.add (Flow.to_event p e) n.flow
    else raise (UnknownEvent e)
  else raise (UnknownPlace p)
      
let set_marking m n =
  n.marking <- if PlaceSet.subset m n.places then m else n.marking

let inputs_of x n = FlowSet.fold 
  (fun f acc -> NodeSet.add f.source acc) 
  (FlowSet.filter (fun f -> f.target = x) n.flow)
  NodeSet.empty

let outputs_of x n = FlowSet.fold 
  (fun f acc -> NodeSet.add f.target acc) 
  (FlowSet.filter (fun f -> f.source = x) n.flow)
  NodeSet.empty

let inputs_of_place p n = 
  let flows = FlowSet.filter (fun f -> Node.of_place p = f.target) n.flow in
  FlowSet.fold 
    (fun f acc -> EventSet.add (Flow.source_event f) acc) 
    flows
    EventSet.empty

let outputs_of_place p n = 
  let flows = FlowSet.filter (fun f -> Node.of_place p = f.source) n.flow in
  FlowSet.fold 
    (fun f acc -> EventSet.add (Flow.target_event f) acc) 
    flows
    EventSet.empty

let inputs_of_event e n =
  let flows = FlowSet.filter (fun f -> Node.of_event e = f.target) n.flow in
  FlowSet.fold 
    (fun f acc -> PlaceSet.add (Flow.source_place f) acc) 
    flows
    PlaceSet.empty

let outputs_of_event e n =
  let flows = FlowSet.filter (fun f -> Node.of_event e = f.source) n.flow in
  FlowSet.fold 
    (fun f acc -> PlaceSet.add (Flow.target_place f) acc) 
    flows
    PlaceSet.empty

let enables m e n = PlaceSet.subset (inputs_of_event e n) m

(* If the current marking of n enables e, fires e and updates n's marking *)
let fire e n = if enables n.marking e n then
  let input = inputs_of_event e n in
  let output = outputs_of_event e n in
  n.marking <- PlaceSet.union (PlaceSet.diff n.marking input) output

let list_of_marking n = PlaceSet.elements n.marking

exception NotANode of string

let is_occurrence_sequence es n =
  let rec helper elist m = match elist with
    [] -> true
  | e::es' ->
      if EventSet.mem e n.events then
        let input = inputs_of_event e n in
        let output = outputs_of_event e n in
        let m' = PlaceSet.union (PlaceSet.diff m input) output in
        enables m e n && helper es' m'
      else
        raise (NotANode e)

  in helper es n.marking

let fire_sequence es n =
  assert (is_occurrence_sequence es n);
  List.fold_left (fun _ e -> fire e n) () es
  
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
    CP := {p | p ∈ C(x)⋂C(y) and IsPlace(p)} // common predecessor places

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

let nodes_of_places ps = PlaceSet.fold
    (fun p acc -> NodeSet.add (Node.of_place p) acc)
    ps
    NodeSet.empty

let nodes_of_events es = EventSet.fold
  (fun e acc -> NodeSet.add (Node.of_event e) acc)
  es
  NodeSet.empty

let concurrencies n =
  let nodes = 
    NodeSet.union (nodes_of_places n.places) (nodes_of_events n.events)
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

exception IllegalGlobalEvent

(* Product of two nets given a synchronization constraint **on the events**.
   The synchronization constraint is represented as a list of pairs of event
   options. The first (second) component of a pair is an event of the lhs (rhs)
   operand or None if that operand doesn't participate in the new transition.
   *)
let product (n1 : t) (n2 : t) (sync : (Event.t option * Event.t option) list) =
  let parse_preflow epair =
    (* Gather all input places of the global transition *)
    let preset = match epair with
      | None, None -> raise IllegalGlobalEvent
      | Some e, None -> inputs_of_event e n1
      | None, Some e -> inputs_of_event e n2
      | Some e1, Some e2 -> 
          PlaceSet.union (inputs_of_event e1 n1) (inputs_of_event e2 n2)

    in PlaceSet.fold
      (fun p acc -> FlowSet.add (p @--> Event.of_pair epair) acc)
      preset
      FlowSet.empty
  in

  let parse_postflow epair =
    (* Gather all output places of the global transition *)
    let postset = match epair with
      | None, None -> raise IllegalGlobalEvent
      | Some e, None -> outputs_of_event e n1
      | None, Some e -> outputs_of_event e n2
      | Some e1, Some e2 -> 
          PlaceSet.union (outputs_of_event e1 n1) (outputs_of_event e2 n2)

    in PlaceSet.fold
      (fun p acc -> FlowSet.add (Event.of_pair epair -->@ p) acc)
      postset
      FlowSet.empty
  in

  {
    places = PlaceSet.union n1.places n2.places;

    events = List.fold_left
      (fun eset epair -> EventSet.add (Event.of_pair epair) eset)
      EventSet.empty
      sync;
      
    flow = List.fold_left
      (fun fset epair -> FlowSet.union
          (FlowSet.union (parse_preflow epair) (parse_postflow epair)) fset)
      FlowSet.empty
      sync;

    marking = PlaceSet.union n1.marking n2.marking;
  }
