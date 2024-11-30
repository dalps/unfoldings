open Petrilib
open Utils

let debug = false

module Make (P : Petrinet.S) = struct
  module PTNet = P
  module O = Occurrence_net.Make (P.Place) (P.Trans)
  module OccurrenceNet = O
  open O

  let tokens_of_places step states history =
    PlaceSet.of_list (List.map (Token.build ~name:step history) states)

  let places_of_tokens = PlaceSet.lift_map Token.label (module P.PlaceSet)

  let places_labeled_as st places =
    PlaceSet.filter (fun p -> Token.label p = st) places

  let extend ?(step = 0) e postset n preset =
    assert (PlaceSet.subset preset (places n));
    let n' = copy n in
    add_trans e n';
    PlaceSet.iter (fun p -> add_to_trans_arc p e n') preset;
    let places_of_postset =
      tokens_of_places step (P.PlaceSet.elements postset) (Event.history e)
    in
    add_places places_of_postset n';
    PlaceSet.iter (fun p -> add_to_place_arc e p n') places_of_postset;
    n'

  let unfold_init (net : P.t) =
    let n0 = init () in
    let initial_marking =
      P.PlaceSet.lift_map (Token.build []) (module PlaceSet) (P.marking net)
    in
    add_places initial_marking n0;
    set_marking initial_marking n0;
    n0

  module UnfoldResult = struct
    type t = { event : Event.t; prefix : O.t }

    let compare = compare
  end

  let unfold1 n step net =
    let module PowerPlaceSet = SetUtils.Make (PlaceSet) in
    let candidates t n =
      (* Consider the input places of the transition in question *)
      P.preset_t net t
      (* Map every input place to the conditions that are labeled by it *)
      |> P.PlaceSet.lift_map
           (fun input -> places_labeled_as input (places n))
           (module PowerPlaceSet)
      (* Compute all possible combinations of the conditions found in the
         previous step, picking one place from each set of inputs.

        The output of this step is again a set of [PlaceSet]s,
        wherein the cardinality of every is the of input places of the transition.
      *)
      |> PlaceSet.combinations (module PowerPlaceSet)
      (* Keep only the reachable markings *)
      |> PowerPlaceSet.filter (fun c -> is_reachable c n)
      (* This is _very_ inefficient :/ *)
    in

    P.TransSet.fold
      (fun t acc ->
        (candidates t n |> PowerPlaceSet.elements
        |> List.filter_map (fun c ->
               let e = Event.build step (past_word_of_preset c n t) t in
               if not (TransSet.mem e (transitions n)) then
                 let n' = extend ~step e (P.postset_t net t) n c in
                 Some { UnfoldResult.event = e; UnfoldResult.prefix = n' }
               else
                 None))
        @ acc)
      (P.transitions net) []

  module Extensions = struct
    module Elt = struct
      type t = Extension of UnfoldResult.t | Leftover of UnfoldResult.t

      let is_extension = function
        | Extension _ -> true
        | _ -> false
      let untag = function
        | Extension r | Leftover r -> r

      let compare e1 e2 =
        let r1, r2 = (untag e1, untag e2) in
        compare (Event.history r1.event) (Event.history r2.event)
    end

    module UnfoldResultSet = SetUtils.Make (UnfoldResult)
    module EltSet = SetUtils.Make (Elt)

    type s = { mutable pool : EltSet.t }

    let empty () = { pool = EltSet.empty }

    let update cond n step ext net =
      assert (not (EltSet.exists Elt.is_extension ext.pool));

      let new_xts = unfold1 n step net in

      (* Add all results of the unfolding as Extension tagged elements *)
      let pool =
        List.fold_right
          (fun r acc -> EltSet.add (Elt.Extension r) acc)
          new_xts EltSet.empty
      in

      (* Combine with previous elements (Leftovers) *)
      let pool = EltSet.union pool ext.pool in (* performance dips if you swap arguments *)

      let pool = EltSet.filter (fun e -> cond (Elt.untag e)) pool in

      if not (EltSet.is_empty pool) then (
        let e = List.hd (EltSet.elements pool) in

        let result =
          match e with
          | Extension r ->
              if debug then
                Printf.printf "%d [Fresh]\n" step;
              r
          | Leftover r ->
              if debug then
                Printf.printf " %d [Leftover from step %d]\n" step
                  (Event.name r.event);
              {
                event =
                  Event.build step (Event.history r.event) (Event.label r.event);
                prefix = union n r.prefix;
                (* update event name in r.prefix!!! *)
              }
        in

        let pool = EltSet.remove e pool in
        ext.pool <- EltSet.remove e ext.pool;

        let conflicts =
          EltSet.filter
            (fun e' ->
              let r, r' = (Elt.untag e, Elt.untag e') in
              not
                (PlaceSet.is_empty
                   (PlaceSet.inter
                      (preset_t r'.prefix r'.event)
                      (preset_t r.prefix r.event))))
            (EltSet.filter Elt.is_extension pool)
        in

        let new_leftovers =
          EltSet.fold
            (fun e' -> EltSet.add (Leftover (Elt.untag e')))
            conflicts EltSet.empty
        in

        ext.pool <- EltSet.union ext.pool new_leftovers;

        Some result)
      else
        None
  end

  let unfold steps net =
    let u0 = unfold_init net in
    let rec helper ext n i net =
      if i > steps then
        n
      else
        match Extensions.update (fun _ -> true) n i ext net with
        | None -> n
        | Some r -> helper ext r.prefix (i + 1) net
    in
    let res = helper (Extensions.empty ()) u0 1 net in
    set_marking (marking u0) res;
    res

  type strategy = P.trans list -> P.trans list -> int

  module type SearchScheme = sig
    (* assumption: e is feasible *)
    val is_terminal : Event.t -> t -> strategy -> P.trans list -> bool

    (* assumption: e is feasible *)
    val is_successful : Event.t -> t -> strategy -> P.trans list -> bool
  end

  module TestResult = struct
    type t = {
      res : bool;
      prefix : O.t;
      history : P.Trans.t list;
      terms : TransSet.t;
    }
  end

  module Tester (SS : SearchScheme) = struct
    let test net stgy goals max_steps =
      let is_feasible e n terms =
        TransSet.is_empty (TransSet.inter (past_conf e n) terms)
      in
      let n0 = unfold_init net in
      let terms0 = TransSet.empty in
      let rec unfold ext step n terms =
        if step > max_steps then
          { TestResult.res = false; prefix = n; history = []; terms }
        else
          match
            Extensions.update
              (fun r -> is_feasible r.event r.prefix terms)
              n step ext net
          with
          | None ->
              let _ = set_marking (marking n0) n in
              { res = false; prefix = n; history = []; terms }
          | Some r ->
              if SS.is_successful r.event r.prefix stgy goals then
                let _ = set_marking (marking n0) n in
                {
                  TestResult.res = true;
                  prefix = r.prefix;
                  history = Event.history r.event;
                  terms;
                }
              else
                let terms' =
                  if SS.is_terminal r.event r.prefix stgy goals then
                    TransSet.add r.event terms
                  else
                    terms
                in
                unfold ext (step + 1) r.prefix terms'
      in
      unfold (Extensions.empty ()) 1 n0 terms0
  end
end
