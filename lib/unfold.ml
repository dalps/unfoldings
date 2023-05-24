open Branching_process
open Product_pretrinet


(* convert a list of states to a set of labeled places with serial naming *)
let label_states states =
  PlaceSet.of_list (List.mapi Labelled_place.build states)


(* filter the places labeled as a certain state in a given set *)
let places_labeled_as st places =
  PlaceSet.filter (fun p -> Labelled_place.label p = st) places


(* extend a branching process n with a new event labeled t and connect it to 
   its preset places c *)
let extend 
  (t : Product_transition.t) (* transition to be added as new event*)
  (name : int) (* the name to assign to the event *)
  (postset : StateSet.t) (* output states to be added as new places *)
  (n : BPNet.t) (* a branching process *)
  (preset : PlaceSet.t) (* a reachable marking of n labeled as *t *)
  : BPNet.t =

  assert (PlaceSet.subset preset (BPNet.places n));

  (* make a copy of n to extend with t *)
  let n' = BPNet.copy n in
  let places_of_postset = label_states (StateSet.elements postset) in
  let event_of_t = Event.build name t in

  BPNet.add_trans event_of_t n';

  BPNet.add_places places_of_postset n';

  PlaceSet.fold
    (fun p _ -> BPNet.add_to_trans_arc p event_of_t n')
    preset
    ();

  PlaceSet.fold
    (fun p _ -> BPNet.add_to_place_arc event_of_t p n')
    places_of_postset
    ();

  n'


(* n0 is the branching process with no events and one place for each component
   of prod, labeled with the initial state of the component *)
let unfold_init (prod : PNet.t) =
  let n0 = BPNet.empty () in
  BPNet.add_places (label_states (StateSet.elements (PNet.marking prod))) n0;
  n0


(* to add events: if in the current (nth) branching process a reachable marking 
   enables a transition t of the product, add a new event n labeled by t and new
   places labeled with the states of t (there can be many transitions enabled
   at once, choose one with the highest priority with respect to the search
   strategy.). Products and branching process types share the same place
   representation (State.t) *)
let unfold_1
  (n : BPNet.t)
  (step : int)
  (prod : PNet.t)
  (stgy : Product_transition.t list -> Product_transition.t list -> int) =

  let n0 = BPNet.empty () in
  let open Product_pretrinet in
  BPNet.add_places (label_states (StateSet.elements (PNet.marking prod))) n0;

  let terminals : EventSet.t = EventSet.empty in

  (* get the reachable markings labeled by *t *)
  let candidates t n =
    let inputs_of_t = PNet.inputs_of_trans t prod in

    (* there's one and only one input state for each participating component *)
    (* for each input state, compute the set of places labeled by it *)
    let options =
      StateSet.fold
      (fun st acc -> (places_labeled_as st (BPNet.places n))::acc)
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
      [] -> []
    | [o] -> PlaceSet.fold
        (fun p acc -> PlaceSet.singleton p::acc)
        o
        []
    | o::options -> add_places o (helper options)

    in helper options

  in List.filter (fun c -> is_reachable c n0) (* ... what t to choose? *)

  (* for each transition of the product, compute its reachable candidates, then
    sort all the histories obtained by appending the transition to the candidate
    and select the smallest according to the search strategy. *)
  (* given two reachable candidates c1 and c2 for a transition t, add t to the the
    candidate with the smaller history -> need a way to associate a history to a 
    marking *)

  (* ALGORITHM SKETCH *)
  (* 1. initialize an empty list l 
     2. for each transition t of prod, compute its reachable candidates in n
     3. for each candidate c of t, let n' be the branching process obtained by
        extending n with an event e named by step and labeled by t
      3.1 add the pair h(e),n' to l
     4. choose from l the branching process with the smallest associated history
        as the result of unfold_1 *)
