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
let extend 
  (e : Event.t) (* a transition to be added as a new event*)
  (postset : StateSet.t) (* the output states to be added as new places *)
  (n : BPNet.t) (* a branching process *)
  (preset : PlaceSet.t) (* a reachable marking of n labeled as *t *)
  : BPNet.t =

  assert (PlaceSet.subset preset (BPNet.places n));

  (* make a copy of n to extend with t *)
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
    history : Product_transition.t list;
    prefix : BPNet.t
  }

  let compare = compare
end

(* to add events: if in the current (nth) branching process a reachable marking 
   enables a transition t of the product, add a new event n labeled by t and new
   places labeled with the states of t (there can be many transitions enabled
   at once, choose one with the highest priority with respect to the search
   strategy.). Products and branching process types share the same place
   representation (State.t) *)
let unfold_1
  (n : BPNet.t)
  (step : int)
  (prod : PNet.t) =

  (* get the reachable markings labeled by *t *)
  let candidates t n =
    let inputs_of_t = PNet.inputs_of_trans t prod in

    (* there's one and only one input state for each participating component *)
    (* for each input state, compute the set of places labeled by it *)
    let options =
      StateSet.fold
      (fun st acc -> (places_labeled_as st (BPNet.marking n))::acc)             (* you could also pick candidates in the difference of the places and the marking of the previous step *)
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

  in

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
        as the result of unfold_1 (need to sort the list) *)

  let possible_extensions = 
    TransSet.fold
      (fun t acc -> List.fold_right
        (fun c acc' ->
          let e = (Event.build step (past_word_of_preset c n t) t) in
          let n' = extend e (PNet.outputs_of_trans t prod) n c in
            (BPNet.fire e n';
            let open UnfoldResult in
            {event = e; history = past_word e n'; prefix = n'}::acc')
        ) 
        (List.filter (fun c -> is_reachable c n) (candidates t n))
        acc
      )
      (PNet.transitions prod)
      []
    
  in possible_extensions


let is_executable prod stgy goals max_steps =
  (

  let module Extensions = struct
    module Elt = struct
      type t = Extension of UnfoldResult.t | Leftover of UnfoldResult.t

      let is_extension = function Extension _ -> true | _ -> false

      let untag = function Extension r | Leftover r -> r

      let compare e1 e2 = let r1, r2 = untag e1, untag e2 in
        stgy r1.history r2.history

      let string_of_elt = function
          Extension r -> "E " ^ (Event.label r.event)
        | Leftover r -> "L " ^ (Event.label r.event)
    end
  
    module UnfoldResultSet = Set.Make(UnfoldResult)
    module EltSet = Set.Make(Elt)
  
    type s = {
      mutable pool : EltSet.t;
    }
  
    let empty () = {
      pool = EltSet.empty;
    }

    let print_pool pool = print_string "{";
      (EltSet.iter (fun e -> print_string (Elt.string_of_elt e ^ "; ")) pool);
      print_endline "}"
  
    (* ALGORITH SKETCH 
       1. get list of extensions
       2. sort by strategy
       3. pick the first one
        3.1 if this is a leftover, update it by "fusing it" with the given prexif
       4. check for conflicting extensions against the chosen one. If any, add them
          to the tagged set as leftovers *)
    let update_if cond n step ext =
      assert (not (EltSet.exists Elt.is_extension ext.pool));

      let new_xts = unfold_1 n step prod in

      (* Add all results of the unfolding as Extension tagged elements *)
      let pool = List.fold_right
        (fun r acc -> EltSet.add (Elt.Extension r) acc)
        new_xts
        EltSet.empty
      in

      (if new_xts = [] then
        (print_endline "\nThe product cannot be unfolded any further!")
      else
        (print_string ("Discovered " ^ string_of_int (List.length new_xts) ^ " fresh candidate extensions: "); print_pool pool; print_endline ""));

      (if not (EltSet.is_empty ext.pool) then
        (print_string "There are leftover candidates to consider: "; print_pool ext.pool; print_endline ""));

      (* Combine with previous elements (Leftovers) *)
      let pool = EltSet.union pool ext.pool in

      let pool = EltSet.filter
        (fun e -> cond (Elt.untag e))
        pool
      in

      if not (EltSet.is_empty pool) then
        let _ = print_string "\nFeasible candidates (sorted by strategy): "; print_pool pool in

        let e = List.hd (EltSet.elements pool) in

        print_endline ("\n[+] Choosing " ^ Elt.string_of_elt e ^ "\n");

        let result = match e with
            Extension r -> r
          | Leftover r -> {
            event =
              Event.build step (Event.history r.event) (Event.label r.event);
            prefix = union n r.prefix;
            history = r.history
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

        (if not (EltSet.is_empty conflicts) then
          (print_string (string_of_int (EltSet.cardinal conflicts) ^ " candidates are in conflict with the selected one: "); print_pool conflicts; print_endline "I'll remember them in future steps as leftovers...\n"));

        let conflicts = EltSet.fold
          (fun e' acc -> EltSet.add (Leftover (Elt.untag e')) acc)
          conflicts
          EltSet.empty in

        ext.pool <- EltSet.union ext.pool conflicts;

        Some result
        
      else
        let _ = print_endline "\nThere are no feasible extensions!" in
        None
  end in

  let n0 = unfold_init prod in
  let terms0 = EventSet.empty in
  let ext = Extensions.empty () in
 
  let is_feasible e n terms =
    let b = EventSet.is_empty (EventSet.inter (past_conf e n) terms) in
    print_endline ("Is " ^ Event.label e ^ " (found in " ^ string_of_int (Event.name e) ^ ") feasible? -> " ^ string_of_bool b);
    b 
  in

  (* assumption: e is feasible *)
  let is_terminal_a e =
    let b = List.mem (Event.label e) goals in
    print_endline ("Is " ^ Event.label e ^ " successful? -> " ^ string_of_bool b);
    b
  in

  (* assumption: e is feasible *)
  let is_terminal_b e n =
    let b = EventSet.exists
      (fun e' -> stgy (past_word e' n) (past_word e n) < 0 && StateSet.equal
        (labels_of_places (BPNet.outputs_of_trans e' n)) 
        (labels_of_places (BPNet.outputs_of_trans e n)))
      (BPNet.transitions n)

    in
    print_endline ("Is " ^ Event.label e ^ " terminal? -> " ^ string_of_bool b);
    b
  in

  let rec helper step n terms : bool =
    step <= max_steps && (

    print_string ("\n+------------------+\n       ");
    print_string ("STEP " ^ string_of_int step);
    print_endline ("       \n+------------------+\n");
    print_string ("Search space (" ^ string_of_int (PlaceSet.cardinal (BPNet.marking n)) ^ " places): ");
    print_placeset (BPNet.marking n);

    let choice = Extensions.update_if
      (fun (r : UnfoldResult.t) -> is_feasible r.event r.prefix terms)
      n
      step
      ext
    in

    (match choice with
        Some r ->
          (* if r.event is a terminal of type (a) end the search successfully *)
          is_terminal_a r.event || (      
            let terms' = if is_terminal_b r.event r.prefix then 
              EventSet.add r.event terms else terms
            in helper (step+1) r.prefix terms')

      | None ->
          (print_string "\nNone of ["; List.iter (fun t -> print_string (t ^ "; ") ) goals; print_endline "] is executable!");
          false))

  in helper 1 n0 terms0)