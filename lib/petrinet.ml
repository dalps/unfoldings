module Make(P : Set.OrderedType) (T : Set.OrderedType) =
  struct

    module Node = struct
      type t = P of P.t | T of T.t

      exception NotAPlace
      exception NotATransition
    
      let of_place p = P p
      let of_trans e = T e      
      let is_place = function P _ -> true | T _ -> false
      let is_trans = function P _ -> false | T _ -> true
      let place_of = function P p -> p | _ -> raise NotAPlace
      let trans_of = function T e -> e | _ -> raise NotATransition
      
      let compare = compare
    end
    

    module Flow = struct
      type t = {source: Node.t; target: Node.t} 
      
      let to_place e p = {source = Node.of_trans e; target = Node.of_place p}
      let to_trans p e = {source = Node.of_place p; target = Node.of_trans e}     
      let source f = f.source
      let target f = f.target
      let target_trans f = Node.trans_of f.target
      let source_trans f = Node.trans_of f.source
      let target_place f = Node.place_of f.target
      let source_place f = Node.place_of f.source

      let compare = compare
    end

    let (-->@) = Flow.to_place
    let (@-->) = Flow.to_trans
        
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
    
    let of_lists ps ts fs im = {
      places = PSet.of_list ps;
      transitions = TSet.of_list ts;
      flow = FlowSet.of_list fs;
      marking = PSet.of_list im;
    }

    let of_sets pset tset fset im = {
      places = pset;
      transitions = tset;
      flow = fset;
      marking = im
    }

    let copy n = of_sets n.places n.transitions n.flow n.marking

    let places n = n.places
    let transitions n = n.transitions
    let flow n = n.flow
    let marking n = n.marking
    
    let add_place p n = n.places <- PSet.add p n.places
    
    let add_trans t n = 
      n.transitions <- TSet.add t n.transitions
    
    let add_places ps n = n.places <- PSet.union ps n.places
    
    let add_transs ts n =
      n.transitions <- TSet.union (TSet.of_list ts) n.transitions
    
    exception UnknownPlace of P.t
    exception UnknownTransition of T.t
    exception NotANode of Node.t
    
    let add_to_place_arc t p n = 
      if PSet.mem p n.places 
      then
        if TSet.mem t n.transitions
        then n.flow <- FlowSet.add (Flow.to_place t p) n.flow
        else raise (UnknownTransition t)
      else raise (UnknownPlace p)
    
    let add_to_trans_arc p t n = 
      if PSet.mem p n.places 
      then
        if TSet.mem t n.transitions
        then n.flow <- FlowSet.add (Flow.to_trans p t) n.flow
        else raise (UnknownTransition t)
      else raise (UnknownPlace p)
          
    let set_marking m n =
      n.marking <- if PSet.subset m n.places then m else n.marking

    let nodes_of_places pset = PSet.fold
      (fun p acc -> NodeSet.add (Node.of_place p) acc)
      pset
      NodeSet.empty
  
    let nodes_of_transs tset = TSet.fold
      (fun t acc -> NodeSet.add (Node.of_trans t) acc)
      tset
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
    
    let inputs_of_trans t n =
      let flows = FlowSet.filter (fun f -> Node.of_trans t = f.target) n.flow in
      FlowSet.fold 
        (fun f acc -> PSet.add (Flow.source_place f) acc) 
        flows
        PSet.empty
    
    let outputs_of_trans t n =
      let flows = FlowSet.filter (fun f -> Node.of_trans t = f.source) n.flow in
      FlowSet.fold 
        (fun f acc -> PSet.add (Flow.target_place f) acc) 
        flows
        PSet.empty

    let enables m t n = PSet.subset (inputs_of_trans t n) m

    let fire t n = if enables n.marking t n then
      let input = inputs_of_trans t n in
      let output = outputs_of_trans t n in
      set_marking (PSet.union (PSet.diff n.marking input) output) n

    let is_occurrence_sequence ts n =
      let rec helper tlist m = match tlist with
        [] -> true
      | t::ts' ->
          if TSet.mem t n.transitions then
            let input = inputs_of_trans t n in
            let output = outputs_of_trans t n in
            let m' = PSet.union (PSet.diff m input) output in
            enables m t n && helper ts' m'
          else
            raise (NotANode (Node.of_trans t))
    
      in helper ts n.marking

    let fire_sequence ts n =
      assert (is_occurrence_sequence ts n);
      List.fold_left (fun _ t -> fire t n) () ts
  end
