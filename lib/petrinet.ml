module Make(P : Set.OrderedType) (T : Set.OrderedType) =
  struct

    module Node = struct
      type t = P of P.t | T of T.t

      exception NotAPlace
      exception NotATransition
    
      let is_place = function P _ -> true | T _ -> false
      let is_trans = function P _ -> false | T _ -> true
      let of_place p = P p
      let of_trans e = T e      
      let place_of = function P p -> p | _ -> raise NotAPlace
      let trans_of = function T e -> e | _ -> raise NotATransition
      let compare = compare
    end
    

    module Flow = struct
      type t = {source: Node.t; target: Node.t} 
    
      exception IllegalFlow
      
      let build src tgt =
        if
          Node.is_place src && Node.is_trans tgt ||
          Node.is_trans src && Node.is_place tgt
        then
          {source = src; target = tgt}
        else
          raise IllegalFlow
    
      let source f = f.source
      let target f = f.target
          
      let to_place e p = {source = Node.of_trans e; target = Node.of_place p}
      let to_trans p e = {source = Node.of_place p; target = Node.of_trans e}

      let target_trans f = Node.trans_of f.target
      let source_trans f = Node.trans_of f.source
      let target_place f = Node.place_of f.target
      let source_place f = Node.place_of f.source

      let compare = compare
    end
    
    let (@-->) = Flow.to_trans
    let (-->@) = Flow.to_place
    
    module PSet = Set.Make(P)
    module TSet = Set.Make(T)
    module FlowSet = Set.Make(Flow)
    module NodeSet = Set.Make(Node)
    
    type t = {
      mutable places: PSet.t;
      mutable transitions: TSet.t;
      mutable flow: FlowSet.t;
      mutable marking: PSet.t
    }
    
    let empty () = {
      places = PSet.empty;
      transitions = TSet.empty;
      flow = FlowSet.empty;
      marking = PSet.empty
    }
    
    let build ps ts fs im = {
        places = PSet.of_list ps;
        transitions = TSet.of_list ts;
        flow = FlowSet.of_list fs;
        marking = PSet.of_list im;
      }

    let build2 pset tset fset im =
      {
        places = pset;
        transitions = tset;
        flow = fset;
        marking = im
      }

    let places n = n.places
    let transitions n = n.transitions

    let flow n = n.flow
    let marking n = n.marking
    
    let add_place p n = n.places <- PSet.add p n.places
    
    let add_trans e n = 
      n.transitions <- TSet.add e n.transitions
    
    let add_places ps n = n.places <- PSet.union ps n.places
    
    let add_transs es n =
      n.transitions <- TSet.union (TSet.of_list es) n.transitions
    
    exception UnknownPlace of P.t
    exception UnknownTransition of T.t
    
    let add_to_place_arc e p n = 
      if PSet.mem p n.places 
      then
        if TSet.mem e n.transitions
        then n.flow <- FlowSet.add (Flow.to_place e p) n.flow
        else raise (UnknownTransition e)
      else raise (UnknownPlace p)
    
    let add_to_trans_arc p e n = 
      if PSet.mem p n.places 
      then
        if TSet.mem e n.transitions
        then n.flow <- FlowSet.add (Flow.to_trans p e) n.flow
        else raise (UnknownTransition e)
      else raise (UnknownPlace p)
          
    let set_marking m n =
      n.marking <- if PSet.subset m n.places then m else n.marking


    let nodes_of_places ps = PSet.fold
      (fun p acc -> NodeSet.add (Node.of_place p) acc)
      ps
      NodeSet.empty
  
    let nodes_of_transs es = TSet.fold
      (fun e acc -> NodeSet.add (Node.of_trans e) acc)
      es
      NodeSet.empty
    
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
        (fun f acc -> TSet.add (Flow.source_trans f) acc) 
        flows
        TSet.empty
    
    let outputs_of_place p n = 
      let flows = FlowSet.filter (fun f -> Node.of_place p = f.source) n.flow in
      FlowSet.fold 
        (fun f acc -> TSet.add (Flow.target_trans f) acc) 
        flows
        TSet.empty
    
    let inputs_of_trans e n =
      let flows = FlowSet.filter (fun f -> Node.of_trans e = f.target) n.flow in
      FlowSet.fold 
        (fun f acc -> PSet.add (Flow.source_place f) acc) 
        flows
        PSet.empty
    
    let outputs_of_trans e n =
      let flows = FlowSet.filter (fun f -> Node.of_trans e = f.source) n.flow in
      FlowSet.fold 
        (fun f acc -> PSet.add (Flow.target_place f) acc) 
        flows
        PSet.empty
  end
