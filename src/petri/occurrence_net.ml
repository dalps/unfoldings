module type S = sig
  type place1
  type trans1

  module Token : sig
    type t = { name : int; history : trans1 list; label : place1 }

    val build : ?name:int -> trans1 list -> place1 -> t
    val name : t -> int
    val history : t -> trans1 list
    val label : t -> place1
    val compare : t -> t -> int
  end

  module Event : sig
    type t = { name : int; history : trans1 list; label : trans1 }

    val build : int -> trans1 list -> trans1 -> t
    val name : t -> int
    val history : t -> trans1 list
    val label : t -> trans1
    val compare : t -> t -> int
  end

  include module type of Petrinet.Make (Token) (Event)

  val predecessors : NodeSet.elt -> t -> NodeSet.t
  val parents_of_event : trans -> t -> TransSet.t
  val past : trans -> t -> TransSet.elt list
  val past_word : trans -> t -> trans1 list
  val past_conf : TransSet.elt -> t -> TransSet.t
  val past_of_preset : PlaceSet.t -> t -> TransSet.elt list
  val past_word_of_preset : PlaceSet.t -> t -> trans1 -> trans1 list
  val place_history : PlaceSet.elt -> t -> trans1 list
  val is_predecessor : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_causally_related : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_conflict : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_concurrent : NodeSet.elt -> NodeSet.elt -> t -> bool
  val is_reachable : PlaceSet.t -> t -> bool
  val union : t -> t -> t
end

module Make (P : Set.OrderedType) (T : Set.OrderedType) = struct
  type place1 = P.t
  type trans1 = T.t

  (* expose Token and Event outside of Make *)
  module Token = struct
    type t = { name : int; history : T.t list; label : P.t }

    let build ?(name = 0) history label = { name; history; label }
    let name p = p.name
    let history p = p.history
    let label p = p.label

    let compare p1 p2 =
      let n = compare p1.history p2.history in
      if n = 0 then compare p1.label p2.label else n
  end

  module Event = struct
    type t = { name : int; history : T.t list; label : T.t }

    let build name history label = { name; history; label }
    let name e = e.name
    let history e = e.history
    let label e = e.label

    let compare e1 e2 =
      let n = compare e1.history e2.history in
      if n = 0 then compare e1.label e2.label else n
  end

  include Petrinet.Make (Token) (Event)

  let predecessors x n =
    let rec helper p parents =
      NodeSet.fold
        (fun parent acc ->
          if NodeSet.mem parent acc then acc
          else helper parent (NodeSet.add parent acc))
        (preset_x n p) parents
    in

    helper x NodeSet.empty

  let parents_of_event e n =
    PlaceSet.fold
      (fun p acc -> TransSet.union (preset_p n p) acc)
      (preset_t n e) TransSet.empty

  let past e n =
    let rec helper p word =
      let parents_of_p = parents_of_event p n in
      TransSet.fold
        (fun parent acc ->
          if List.mem parent acc then acc
          else helper parent (TransSet.elements parents_of_p @ acc))
        parents_of_p word
    in

    helper e [] @ [ e ]
  (* e itself is part of its past *)

  let past_word e n = List.map Event.label (past e n)

  let past_conf e n =
    TransSet.add e
      (NodeSet.fold
         (fun x acc -> TransSet.add (Node.trans_of x) acc)
         (NodeSet.filter Node.is_trans (predecessors (Node.of_trans e) n))
         TransSet.empty)

  let past_of_preset ps n =
    let parent_events =
      PlaceSet.fold
        (fun p acc ->
          let input_event = preset_p n p in
          assert (TransSet.cardinal input_event <= 1);
          TransSet.union input_event acc)
        ps TransSet.empty
    in

    let rec helper (ps : TransSet.t) word =
      TransSet.fold
        (fun p acc ->
          let parents_of_p = parents_of_event p n in
          TransSet.fold
            (fun parent acc' ->
              if List.mem parent acc' then acc'
              else helper parents_of_p (TransSet.elements parents_of_p @ acc'))
            parents_of_p acc)
        ps word
    in

    helper parent_events (TransSet.elements parent_events)

  let past_word_of_preset ps n t =
    List.map Event.label (past_of_preset ps n) @ [ t ]

  let place_history p n =
    match TransSet.elements (preset_p n p) with
    | [] -> []
    | e :: _ -> past_word e n

  let is_predecessor x y n = NodeSet.mem x (predecessors y n)
  let is_causally_related x y n = is_predecessor x y n || is_predecessor y x n

  let is_conflict x y n =
    let pred_x = predecessors x n in
    let pred_y = predecessors y n in
    let pred_x_only = NodeSet.add x (NodeSet.diff pred_x pred_y) in
    let pred_y_only = NodeSet.add y (NodeSet.diff pred_y pred_x) in
    let pred_places_in_common =
      NodeSet.filter Node.is_place (NodeSet.inter pred_x pred_y)
    in

    NodeSet.exists
      (fun p ->
        let es = postset_x n p in
        NodeSet.exists
          (fun e ->
            NodeSet.mem e pred_x_only
            && NodeSet.exists
                 (fun e' -> e <> e' && NodeSet.mem e' pred_y_only)
                 es
            || NodeSet.mem e pred_y_only
               && NodeSet.exists
                    (fun e' -> e <> e' && NodeSet.mem e' pred_x_only)
                    es)
          es)
      pred_places_in_common

  let is_concurrent x y n =
    (not (is_causally_related x y n)) && not (is_conflict x y n)

  let is_reachable m n =
    let nodes = nodeset_of_placeset m in
    NodeSet.for_all
      (fun x -> NodeSet.for_all (fun y -> is_concurrent x y n) nodes)
      nodes

  let union n1 n2 =
    of_sets
      (PlaceSet.union (places n1) (places n2))
      (TransSet.union (transitions n1) (transitions n2))
      (bind_f (preset_t n1) (preset_t n2))
      (bind_f (postset_t n1) (postset_t n2))
      (PlaceSet.union (marking n1) (marking n2))

  let find_event_by_id id n =
    TransSet.find_first (fun e -> Event.name e >= id) (transitions n)

  let find_events_by_id id n =
    List.filter (fun p -> Event.name p = id) (TransSet.elements (transitions n))

  let find_event_by_label t n =
    TransSet.find_first (fun e -> Event.label e >= t) (transitions n)

  let find_token_by_id id n =
    PlaceSet.find_first (fun p -> Token.name p >= id) (places n)

  let find_tokens_by_id id n =
    List.filter (fun p -> Token.name p = id) (PlaceSet.elements (places n))

  let find_tokens_by_label s n =
    List.filter (fun p -> Token.label p = s) (PlaceSet.elements (places n))

  let find_token_by_label s n = find_tokens_by_label s n |> List.hd
end
