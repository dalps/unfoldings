module type S = sig
  module PTNet : Petrinet.S
  module OccurrenceNet : Occurrence_net.S

  val tokens_of_places :
    OccurrenceNet.place1 list -> PTNet.trans list -> OccurrenceNet.PlaceSet.t

  val places_of_tokens : OccurrenceNet.PlaceSet.t -> PTNet.PlaceSet.t
  val unfold_init : PTNet.t -> OccurrenceNet.t

  module UnfoldResult : sig
    type t = { event : OccurrenceNet.trans; prefix : OccurrenceNet.t }

    val compare : 'a -> 'a -> int
  end

  val unfold_1 : OccurrenceNet.t -> int -> PTNet.t -> UnfoldResult.t list
  val unfold : OccurrenceNet.t -> int -> PTNet.t -> OccurrenceNet.t

  type strategy = PTNet.trans list -> PTNet.trans list -> int

  module type SearchScheme = sig
    val is_terminal :
      OccurrenceNet.trans ->
      OccurrenceNet.t ->
      strategy ->
      PTNet.trans list ->
      bool

    val is_successful :
      OccurrenceNet.trans ->
      OccurrenceNet.t ->
      strategy ->
      PTNet.trans list ->
      bool
  end

  module Tester : functor (_ : SearchScheme) -> sig
    val test : PTNet.t -> strategy -> PTNet.trans list -> int -> bool
  end
end

module Make (Net : Petrinet.S) = struct
  module PTNet = Net
  module OccurrenceNet = Occurrence_net.Make (Net.Place) (Net.Trans)
  open OccurrenceNet

  let tokens_of_places step states history =
    PlaceSet.of_list (List.map (Token.build ~name:step history) states)

  let places_of_tokens ps =
    PlaceSet.fold
      (fun p acc -> Net.PlaceSet.add (Token.label p) acc)
      ps Net.PlaceSet.empty

  let places_labeled_as st places =
    PlaceSet.filter (fun p -> Token.label p = st) places

  let extend ?(step = 0) e postset n preset =
    assert (PlaceSet.subset preset (places n));
    let n' = copy n in
    add_trans e n';
    PlaceSet.iter (fun p -> add_to_trans_arc p e n') preset;
    let places_of_postset =
      tokens_of_places step (Net.PlaceSet.elements postset) (Event.history e)
    in
    add_places places_of_postset n';
    PlaceSet.iter (fun p -> add_to_place_arc e p n') places_of_postset;
    n'

  let unfold_init (net : Net.t) =
    let n0 = empty () in
    let initial_marking =
      Net.PlaceSet.fold
        (fun s acc -> PlaceSet.add (Token.build [] s) acc)
        (Net.marking net) PlaceSet.empty
    in
    add_places initial_marking n0;
    set_marking initial_marking n0;
    n0

  module UnfoldResult = struct
    type t = { event : Event.t; prefix : OccurrenceNet.t }

    let compare = compare
  end

  let unfold_1 n step net =
    let candidates t n =
      let inputs_of_t = Net.preset_t net t in

      let options =
        Net.PlaceSet.fold
          (fun st acc -> places_labeled_as st (marking n) :: acc)
          inputs_of_t []
      in

      let add_places ps res =
        PlaceSet.fold
          (fun p acc1 ->
            List.fold_right
              (fun set acc2 -> PlaceSet.add p set :: acc2)
              res acc1)
          ps []
      in
      let rec helper = function
        | [] -> []
        | [ o ] -> PlaceSet.fold (fun p acc -> PlaceSet.singleton p :: acc) o []
        | o :: options -> add_places o (helper options)
      in
      helper options
    in
    Net.TransSet.fold
      (fun t acc ->
        List.fold_right
          (fun c acc' ->
            let e = Event.build step (past_word_of_preset c n t) t in
            let n' = extend ~step e (Net.postset_t net t) n c in
            fire e n';
            { UnfoldResult.event = e; UnfoldResult.prefix = n' } :: acc')
          (List.filter (fun c -> is_reachable c n) (candidates t n))
          acc)
      (Net.transitions net) []

  module Extensions = struct
    module Elt = struct
      type t = Extension of UnfoldResult.t | Leftover of UnfoldResult.t

      let is_extension = function Extension _ -> true | _ -> false
      let untag = function Extension r | Leftover r -> r

      let compare e1 e2 =
        let r1, r2 = (untag e1, untag e2) in
        compare (Event.history r1.event) (Event.history r2.event)
    end

    module UnfoldResultSet = Set.Make (UnfoldResult)
    module EltSet = Set.Make (Elt)

    type s = { mutable pool : EltSet.t }

    let empty () = { pool = EltSet.empty }

    let update cond n step ext net =
      assert (not (EltSet.exists Elt.is_extension ext.pool));

      let new_xts = unfold_1 n step net in

      (* Add all results of the unfolding as Extension tagged elements *)
      let pool =
        List.fold_right
          (fun r acc -> EltSet.add (Elt.Extension r) acc)
          new_xts EltSet.empty
      in

      (* Combine with previous elements (Leftovers) *)
      let pool = EltSet.union pool ext.pool in

      let pool = EltSet.filter (fun e -> cond (Elt.untag e)) pool in

      if not (EltSet.is_empty pool) then (
        let e = List.hd (EltSet.elements pool) in

        let result =
          match e with
          | Extension r ->
              print_endline (string_of_int step ^ " [Fresh]");
              r
          | Leftover r ->
              print_endline
                (string_of_int step ^ " [Leftover from step "
                ^ string_of_int (Event.name r.event)
                ^ "]");
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
      else None
  end

  let unfold steps net =
    let u0 = unfold_init net in
    let rec helper ext n i net =
      if i > steps then n
      else
        match Extensions.update (fun _ -> true) n i ext net with
        | None -> n
        | Some r -> helper ext r.prefix (i + 1) net
    in
    helper (Extensions.empty ()) u0 1 net

  type strategy = Net.TransSet.elt list -> Net.TransSet.elt list -> int

  module type SearchScheme = sig
    (* assumption: e is feasible *)
    val is_terminal : Event.t -> t -> strategy -> Net.Trans.t list -> bool

    (* assumption: e is feasible *)
    val is_successful : Event.t -> t -> strategy -> Net.Trans.t list -> bool
  end

  module TestResult = struct
    type t = { res : bool; prefix : OccurrenceNet.t; terms : TransSet.t }
  end

  module Tester (SS : SearchScheme) = struct
    let test net stgy goals max_steps =
      let is_feasible e n terms =
        TransSet.is_empty (TransSet.inter (past_conf e n) terms)
      in
      let n0 = unfold_init net in
      let terms0 = TransSet.empty in
      let rec unfold ext step n terms =
        if step > max_steps then { TestResult.res = false; prefix = n; terms }
        else
          match
            Extensions.update
              (fun r -> is_feasible r.event r.prefix terms)
              n step ext net
          with
          | None -> { res = false; prefix = n; terms }
          | Some r ->
              if SS.is_successful r.event r.prefix stgy goals then
                { TestResult.res = true; prefix = r.prefix; terms }
              else
                let terms' =
                  if SS.is_terminal r.event r.prefix stgy goals then
                    TransSet.add r.event terms
                  else terms
                in
                unfold ext (step + 1) r.prefix terms'
      in
      unfold (Extensions.empty ()) 1 n0 terms0
  end
end
