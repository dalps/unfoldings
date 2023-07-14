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

  let tokens_of_places states history =
    PlaceSet.of_list (List.map (Token.build history) states)

  let places_of_tokens ps =
    PlaceSet.fold
      (fun p acc -> Net.PlaceSet.add (Token.label p) acc)
      ps Net.PlaceSet.empty

  let places_labeled_as st places =
    PlaceSet.filter (fun p -> Token.label p = st) places

  let extend e postset n preset =
    assert (PlaceSet.subset preset (places n));
    let n' = copy n in
    add_trans e n';
    PlaceSet.iter (fun p -> add_to_trans_arc p e n') preset;
    let places_of_postset =
      tokens_of_places (Net.PlaceSet.elements postset) (Event.history e)
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
          (fun st acc -> places_labeled_as st (places n) :: acc)
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
        List.filter_map
          (fun c ->
            let e = Event.build step (past_word_of_preset c n t) t in
            if not (TransSet.mem e (transitions n)) then
              let n' = extend e (Net.postset_t net t) n c in
              Some { UnfoldResult.event = e; UnfoldResult.prefix = n' }
            else None)
          (List.filter (fun c -> is_reachable c n) (candidates t n))
        @ acc)
      (Net.transitions net) []

  let rec unfold n0 steps net =
    match unfold_1 n0 steps net with
    | _ when steps <= 0 -> n0
    | [] -> n0
    | n :: _ -> unfold n.prefix (steps - 1) net

  type strategy = Net.TransSet.elt list -> Net.TransSet.elt list -> int

  module type SearchScheme = sig
    (* assumption: e is feasible *)
    val is_terminal : Event.t -> t -> strategy -> Net.Trans.t list -> bool

    (* assumption: e is feasible *)
    val is_successful : Event.t -> t -> strategy -> Net.Trans.t list -> bool
  end

  module Tester (SS : SearchScheme) = struct
    let test net stgy goals max_steps =
      let is_feasible e n terms =
        TransSet.is_empty (TransSet.inter (past_conf e n) terms)
      in
      let n0 = unfold_init net in
      let terms0 = TransSet.empty in
      let rec unfold step n terms =
        step <= max_steps
        &&
        let ext =
          List.filter
            (fun (r : UnfoldResult.t) -> is_feasible r.event r.prefix terms)
            (unfold_1 n step net)
        in
        List.length ext <> 0
        &&
        let r = List.hd ext in
        SS.is_successful r.event r.prefix stgy goals
        ||
        let terms' =
          if SS.is_terminal r.event r.prefix stgy goals then
            TransSet.add r.event terms
          else terms
        in
        unfold (step + 1) r.prefix terms'
      in
      unfold 1 n0 terms0
  end
end
