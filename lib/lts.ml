module Trans = struct
  type t = {source : State.t; label : string; target : State.t}
  
  let step tr = (tr.source, tr, tr.target)
  let enables s tr = s = tr.source
  let compare = compare
end

let (-->) s1 s2 lbl = {
  Trans.source = s1;
  Trans.label = lbl;
  Trans.target = s2
}

module StateSet = Set.Make(State)
module TransSet = Set.Make(Trans)

type t = {
  mutable states : StateSet.t;
  transitions : (string, Trans.t) Hashtbl.t;
  mutable initialState : State.t
}

let empty () = {
  states = StateSet.empty;
  transitions = Hashtbl.create 100;
  initialState = ""
}

exception InvalidInitializer

let build ss ts is =
  let sys = empty () in
  let states = StateSet.of_list ss in
  let states_of_ts = List.fold_left
    (fun acc (t : Trans.t) -> StateSet.add t.source (StateSet.add t.target acc))
    StateSet.empty
    ts
  in
  (* All states used to define transitions must be declared in the state list *)
  if StateSet.subset states_of_ts states then
    (sys.states <- states;
    List.fold_left
      (fun _ (t : Trans.t) -> Hashtbl.add sys.transitions t.label t)
      ()
      ts;
    (* The initial state must also be declared in the state list *)
    sys.initialState <- if List.mem is ss then is 
      else raise InvalidInitializer;
    sys)
  else
    raise InvalidInitializer

let add_state s sys = sys.states <- StateSet.add s sys.states

let add_trans (t : Trans.t) sys = 
  let states_of_t = [t.source; t.target] in
  if StateSet.subset (StateSet.of_list states_of_t) sys.states then
    Hashtbl.add sys.transitions t.label t

let set_init_state s sys =
  if StateSet.mem s sys.states then
    sys.initialState <- s

let is_computation (ts : string list) sys = 
  let states = 
    List.fold_left
    (fun acc t -> 
      let trans = Hashtbl.find sys.transitions t
      in match acc with
        [] -> [trans.target]
      | s::_ -> if trans.source = s then trans.target::acc else acc)
    []
    ts

  in List.length states = List.length ts

let is_history (ts : string list) sys =
  let first_t = Hashtbl.find sys.transitions (List.hd ts)
  in first_t.source = sys.initialState && is_computation ts sys

let product_component_of lts (* component_name *) =
  Product_pretrinet.PNet.of_sets
    lts.states

    (Hashtbl.fold
      (fun t _ acc -> Product_pretrinet.TransSet.add t acc)
      lts.transitions
      Product_pretrinet.TransSet.empty)

    (Hashtbl.fold
      (fun t (trans : Trans.t) acc -> 
        let open Product_pretrinet.PNet.Flow in
          Product_pretrinet.PFlowSet.add (trans.source @--> t)
          (Product_pretrinet.PFlowSet.add (t -->@ trans.target) acc))
      lts.transitions
      Product_pretrinet.PFlowSet.empty)

    (StateSet.of_list [lts.initialState])

    (* [ component_name ] *)