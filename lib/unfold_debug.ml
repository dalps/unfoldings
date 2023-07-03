open Branching_process
open Prettyprint
module StateSet = Product.PlaceSet
module GlobalTransSet = Product.TransSet
module LblPlaceSet = Branching_process.PlaceSet
module EventSet = Branching_process.TransSet

(* convert a list of states to a set of labeled places with serial naming *)
let label_states states history =
  LblPlaceSet.of_list (List.map (LabelledPlace.build history) states)

(* filter the places labeled as a certain state in a given set *)
let places_labeled_as st places =
  LblPlaceSet.filter (fun p -> LabelledPlace.label p = st) places

(* get the labels of a set of places *)
let labels_of_places ps =
  LblPlaceSet.fold
    (fun p acc -> StateSet.add (LabelledPlace.label p) acc)
    ps StateSet.empty

(* extend a branching process n with a new event e, by plugging e into a subset
   c of places of n (its preset) and making new places for the output states
   of its associated product transition (its postset) and the necessary arcs *)
let extend e postset n preset =
  assert (LblPlaceSet.subset preset (Branching_process.places n));
  let n' = Branching_process.copy n in
  Branching_process.add_trans e n';
  LblPlaceSet.iter (fun p -> Branching_process.add_to_trans_arc p e n') preset;
  let places_of_postset = label_states (StateSet.elements postset) e.history in
  Branching_process.add_places places_of_postset n';
  LblPlaceSet.iter
    (fun p -> Branching_process.add_to_place_arc e p n')
    places_of_postset;
  n'

(* n0 is the branching process with no events and one place for each component
   of prod, labeled with the initial state of the component *)
let unfold_init (prod : Product.t) =
  let n0 = Branching_process.empty () in
  let initial_marking =
    StateSet.fold
      (fun s acc -> LblPlaceSet.add (LabelledPlace.build [] s) acc)
      (Product.marking prod) LblPlaceSet.empty
  in
  Branching_process.add_places initial_marking n0;
  Branching_process.set_marking initial_marking n0;
  n0

module UnfoldResult = struct
  type t = { event : Event.t; prefix : Branching_process.t }

  let compare = compare
end

let unfold_1 n step prod =
  (* get the reachable markings labeled by *t *)
  let candidates t n =
    let inputs_of_t = Product.preset_t prod t in

    (* for each input state, compute the set of places labeled by it *)
    let options =
      StateSet.fold
        (fun st acc ->
          places_labeled_as st (Branching_process.marking n) :: acc)
        inputs_of_t []
    in

    let add_places (ps : LblPlaceSet.t) (res : LblPlaceSet.t list) :
        LblPlaceSet.t list =
      LblPlaceSet.fold
        (fun p acc1 ->
          List.fold_right
            (fun set acc2 -> LblPlaceSet.add p set :: acc2)
            res acc1)
        ps []
    in
    let rec helper = function
      | [] -> []
      | [ o ] ->
          LblPlaceSet.fold (fun p acc -> LblPlaceSet.singleton p :: acc) o []
      | o :: options -> add_places o (helper options)
    in
    helper options
  in
  let possible_extensions =
    Product.TransSet.fold
      (fun t acc ->
        List.fold_right
          (fun c acc' ->
            let e = Event.build step (past_word_of_preset c n t) t in
            let n' = extend e (Product.postset_t prod t) n c in
            Branching_process.fire e n';
            let open UnfoldResult in
            { event = e; prefix = n' } :: acc')
          (List.filter (fun c -> is_reachable c n) (candidates t n))
          acc)
      (Product.transitions prod) []
  in
  possible_extensions

type strategy = GlobalTransSet.elt list -> GlobalTransSet.elt list -> int

module type SearchScheme = sig
  (* assumption: e is feasible *)
  val is_terminal :
    Event.t -> Branching_process.t -> strategy -> Product.Trans.t list -> bool

  (* assumption: e is feasible *)
  val is_successful :
    Event.t -> Branching_process.t -> strategy -> Product.Trans.t list -> bool
end

module Unfold (SS : SearchScheme) = struct
  let test prod stgy goals max_steps =
    let module Extensions = struct
      module Elt = struct
        type t = Extension of UnfoldResult.t | Leftover of UnfoldResult.t

        let is_extension = function Extension _ -> true | _ -> false
        let untag = function Extension r | Leftover r -> r

        let compare e1 e2 =
          let r1, r2 = (untag e1, untag e2) in
          stgy (Event.history r1.event) (Event.history r2.event)
   
        let string_of_elt = function
          | Extension r -> "E " ^ string_of_trans (Event.label r.event)
          | Leftover r -> "L " ^ string_of_trans (Event.label r.event)
      end

      module UnfoldResultSet = Set.Make (UnfoldResult)
      module EltSet = Set.Make (Elt)

      type s = { mutable pool : EltSet.t }

      let empty () = { pool = EltSet.empty }

      let print_pool pool =
        print_string "{";
        EltSet.iter (fun e -> print_string (Elt.string_of_elt e ^ "; ")) pool;
        print_endline "}"

      let update_if cond n step ext =
        assert (not (EltSet.exists Elt.is_extension ext.pool));

        let new_xts = unfold_1 n step prod in

        (* Add all results of the unfolding as Extension tagged elements *)
        let pool =
          List.fold_right
            (fun r acc -> EltSet.add (Elt.Extension r) acc)
            new_xts EltSet.empty
        in

        if new_xts = [] then
          print_endline "\nThe product cannot be unfolded any further!"
        else (
          print_string
            ("Discovered "
            ^ string_of_int (List.length new_xts)
            ^ " fresh candidate extensions: ");
          print_pool pool;
          print_endline "");
  
        if not (EltSet.is_empty ext.pool) then (
          print_string "There are leftover candidates to consider: ";
          print_pool ext.pool;
          print_endline "");

        (* Combine with previous elements (Leftovers) *)
        let pool = EltSet.union pool ext.pool in

        let pool = EltSet.filter (fun e -> cond (Elt.untag e)) pool in

        if not (EltSet.is_empty pool) then (
          let _ =
            print_string "\nFeasible candidates (sorted by strategy): ";
            print_pool pool
          in

          let e = List.hd (EltSet.elements pool) in
          print_endline ("\n[+] Choosing " ^ Elt.string_of_elt e ^ "\n");

          let result =
            match e with
            | Extension r -> r
            | Leftover r ->
                {
                  event =
                    Event.build step (Event.history r.event)
                      (Event.label r.event);
                  prefix = union n r.prefix;
                }
          in

          let pool = EltSet.remove e pool in
          ext.pool <- EltSet.remove e ext.pool;

          let conflicts =
            EltSet.filter
              (fun e' ->
                let r, r' = (Elt.untag e, Elt.untag e') in
                not
                  (LblPlaceSet.is_empty
                     (LblPlaceSet.inter
                        (Branching_process.preset_t r'.prefix r'.event)
                        (Branching_process.preset_t r.prefix r.event))))
              (EltSet.filter Elt.is_extension pool)
          in

          if not (EltSet.is_empty conflicts) then (
            print_string
              (string_of_int (EltSet.cardinal conflicts)
              ^ " candidates are in conflict with the selected one: ");
            print_pool conflicts;
            print_endline "I'll remember them in future steps as leftovers...\n");

          let conflicts =
            EltSet.fold
              (fun e' acc -> EltSet.add (Leftover (Elt.untag e')) acc)
              conflicts EltSet.empty
          in

          ext.pool <- EltSet.union ext.pool conflicts;

          Some result)
        else
          let _ = print_endline "\nThere are no feasible extensions!" in
          None
    end in
    let is_feasible e n terms =
      let b = EventSet.is_empty (EventSet.inter (past_conf e n) terms) in
      print_endline
        ("Is " ^ string_of_trans (Event.label e) ^ " (found in "
        ^ string_of_int (Event.name e)
        ^ ") feasible? -> " ^ string_of_bool b);
      b
    in
    let n0 = unfold_init prod in
    let terms0 = EventSet.empty in
    let ext = Extensions.empty () in
    let rec unfold step n terms : bool =
      step <= max_steps
      && (
      print_string "\n+------------------+\n       ";
      print_string ("STEP " ^ string_of_int step);
      print_endline "       \n+------------------+\n";
      print_string
        ("Search space ("
        ^ string_of_int (LblPlaceSet.cardinal (marking n))
        ^ " places): ");
      print_endline (string_of_placeset (marking n));
      let choice =
        Extensions.update_if
          (fun (r : UnfoldResult.t) -> is_feasible r.event r.prefix terms)
          n step ext
      in
      match choice with
      | Some r ->
          (* if r.event is a terminal of type (a) end the search successfully *)
          SS.is_successful r.event r.prefix stgy goals
          ||
          let terms' =
            if SS.is_terminal r.event r.prefix stgy goals then
              EventSet.add r.event terms
            else terms
          in
          unfold (step + 1) r.prefix terms'
      | None -> 
        print_string "\nNone of the goals is executable!";
        false)
    in
    unfold 1 n0 terms0
end
