open Branching_process
open Product_pretrinet

(* convert a list of states to a set of labeled places with serial naming *)
let label_states states history =
  PlaceSet.of_list (List.map (Labelled_place.build history) states)

(* filter the places labeled as a certain state in a given set *)
let places_labeled_as st places =
  PlaceSet.filter (fun p -> Labelled_place.label p = st) places

(* get the labels of a set of places *)
let labels_of_places ps = PlaceSet.fold
  (fun p acc -> StateSet.add (Labelled_place.label p) acc)
  ps
  StateSet.empty

(* extend a branching process n with a new event e, by plugging e into a subset
   c of places of n (its preset) and making new places for the output states
   of its associated product transition (its postset) and the necessary arcs *)
let extend e postset n preset =
  assert (PlaceSet.subset preset (BPNet.places n));
  let n' = BPNet.copy n in
  BPNet.add_trans e n';
  PlaceSet.iter (fun p -> BPNet.add_to_trans_arc p e n') preset;
  let places_of_postset = label_states (StateSet.elements postset) e.history in
  BPNet.add_places places_of_postset n';
  PlaceSet.iter (fun p -> BPNet.add_to_place_arc e p n') places_of_postset;
  n'

(* n0 is the branching process with no events and one place for each component
   of prod, labeled with the initial state of the component *)
let unfold_init (prod : PNet.t) =
  let n0 = BPNet.empty () in
  let initial_marking = 
    StateSet.fold
      (fun s acc -> PlaceSet.add (Labelled_place.build [] s) acc)
      (PNet.marking prod)
      PlaceSet.empty
  in
  BPNet.add_places initial_marking n0;
  BPNet.set_marking initial_marking n0;
  n0

module UnfoldResult = struct
  type t = {
    event : Event.t;
    prefix : BPNet.t
  }

  let compare = compare
end

let unfold_1 n step prod =
  (* get the reachable markings labeled by *t *)
  let candidates t n =
    let inputs_of_t = PNet.inputs_of_trans t prod in

    (* for each input state, compute the set of places labeled by it *)
    let options =
      StateSet.fold
      (fun st acc -> (places_labeled_as st (BPNet.marking n))::acc)
      inputs_of_t
      []
    in

    let add_places (ps : PlaceSet.t) (res : PlaceSet.t list) : PlaceSet.t list =
      PlaceSet.fold
      (fun p acc1 -> (List.fold_right
        (fun set acc2 -> (PlaceSet.add p set)::acc2)
        res
        acc1
        ))
      ps
      []
    in
    let rec helper = function
      | [] -> []
      | [o] -> PlaceSet.fold
          (fun p acc -> PlaceSet.singleton p::acc)
          o
          []
      | o::options -> add_places o (helper options)
    in helper options
  in
  let possible_extensions = 
    TransSet.fold
      (fun t acc -> List.fold_right
        (fun c acc' ->
          let e = (Event.build step (past_word_of_preset c n t) t) in
          let n' = extend e (PNet.outputs_of_trans t prod) n c in
            (BPNet.fire e n';
            let open UnfoldResult in
            {event = e; prefix = n'}::acc')
        ) 
        (List.filter (fun c -> is_reachable c n) (candidates t n))
        acc
      )
      (PNet.transitions prod)
      []
  in possible_extensions

let is_executable prod stgy goals max_steps = (
  let module Extensions = struct
    module Elt = struct
      type t = Extension of UnfoldResult.t | Leftover of UnfoldResult.t

      let is_extension = function Extension _ -> true | _ -> false

      let untag = function Extension r | Leftover r -> r

      let compare e1 e2 = let r1, r2 = untag e1, untag e2 in
        stgy (Event.history r1.event) (Event.history r2.event)
    end
  
    module UnfoldResultSet = Set.Make(UnfoldResult)
    module EltSet = Set.Make(Elt)
  
    type s = {
      mutable pool : EltSet.t;
    }
  
    let empty () = {
      pool = EltSet.empty;
    }
  
    let update_if cond n step ext =
      assert (not (EltSet.exists Elt.is_extension ext.pool));

      let new_xts = unfold_1 n step prod in

      (* Add all results of the unfolding as Extension tagged elements *)
      let pool = List.fold_right
        (fun r acc -> EltSet.add (Elt.Extension r) acc)
        new_xts
        EltSet.empty
      in

      (* Combine with previous elements (Leftovers) *)
      let pool = EltSet.union pool ext.pool in

      let pool = EltSet.filter
        (fun e -> cond (Elt.untag e))
        pool
      in

      if not (EltSet.is_empty pool) then
        let e = List.hd (EltSet.elements pool) in

        let result = match e with
            Extension r -> r
          | Leftover r -> {
            event =
              Event.build step (Event.history r.event) (Event.label r.event);
            prefix = union n r.prefix;
          }
        in

        let pool = EltSet.remove e pool in
        ext.pool <- EltSet.remove e ext.pool;

        let conflicts = EltSet.filter 
          (fun e' -> let r, r' = Elt.untag e, Elt.untag e' in
            not (PlaceSet.is_empty 
            (PlaceSet.inter
              (BPNet.inputs_of_trans r'.event r'.prefix)
              (BPNet.inputs_of_trans r.event r.prefix)))) 
          (EltSet.filter Elt.is_extension pool) in

        let conflicts = EltSet.fold
          (fun e' acc -> EltSet.add (Leftover (Elt.untag e')) acc)
          conflicts
          EltSet.empty in

        ext.pool <- EltSet.union ext.pool conflicts;

        Some result
        
      else
        None
    end 
  in

  let n0 = unfold_init prod in
  let terms0 = EventSet.empty in
  let ext = Extensions.empty () in
  let is_feasible e n terms =
    EventSet.is_empty (EventSet.inter (past_conf e n) terms) 
  in
  let is_terminal_a e = (* assumption: e is feasible *)
    List.mem (Event.label e) goals
  in
  let is_terminal_b e n = (* assumption: e is feasible *)
    EventSet.exists
      (fun e' -> stgy (past_word e' n) (past_word e n) < 0 && StateSet.equal
        (labels_of_places (BPNet.outputs_of_trans e' n)) 
        (labels_of_places (BPNet.outputs_of_trans e n)))
      (BPNet.transitions n)
  in
  let rec helper step n terms : bool =
    step <= max_steps && (
    let choice = Extensions.update_if
      (fun (r : UnfoldResult.t) -> is_feasible r.event r.prefix terms)
      n
      step
      ext
    in
    match choice with
    | Some r ->
        (* if r.event is a terminal of type (a) end the search successfully *)
        is_terminal_a r.event || (      
          let terms' = if is_terminal_b r.event r.prefix then 
            EventSet.add r.event terms else terms
          in helper (step+1) r.prefix terms'
        )
    | None -> false)
  in 
    helper 1 n0 terms0)