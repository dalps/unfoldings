module Trans = struct
  type local_t = Idle | T of string | U of string

  type t = local_t list

  let is_idle = (=) Idle

  let participates i t = List.nth t i <> Idle

  let projection i h = List.filter (participates i) h

  let is_independent t1 t2 = List.for_all
    (fun (l1, l2) -> is_idle l1 || is_idle l2)
    (List.combine t1 t2)

  let compare = compare
end

include Petrinet.Make(State)(Trans)

(* Product of a list of nets ns given a synchronization constraint sync,
   represented as a list of list of transition options. The i-th element of
   an global transition of sync corresponds to some transition of the i-th 
   component or None if that component doesn't participate.
   Components must not share names of states and transitions. 
   Component order must match the order of local transitions. *)
let product (ns : t list) (sync : Trans.t list) =
  let parse_preflow (trans : Trans.t) =
    let preset =
      List.fold_left2
        (fun acc n t -> PlaceSet.union (inputs_of_trans [t] n) acc)
        PlaceSet.empty
        ns
        trans
    in PlaceSet.fold
      (fun s acc -> FlowSet.add (s @--> trans) acc)
      preset
      FlowSet.empty
  in

  let parse_postflow trans =
    let postset =
      List.fold_left2
        (fun acc n t -> PlaceSet.union (outputs_of_trans [t] n) acc)
        PlaceSet.empty
        ns
        trans
    in PlaceSet.fold
      (fun s acc -> FlowSet.add (trans -->@ s) acc)
      postset
      FlowSet.empty

  in of_sets
    (List.fold_left
      (fun acc n -> PlaceSet.union (places n) acc)
      PlaceSet.empty
      ns)
    (TransSet.of_list sync)
    (List.fold_left
      (fun acc trans -> FlowSet.union
        (FlowSet.union (parse_preflow trans) (parse_postflow trans)) acc)
      FlowSet.empty
      sync)
    (List.fold_left
      (fun acc n -> PlaceSet.union (marking n) acc)
      PlaceSet.empty
      ns)

let rec undo_of = function
  | [] -> []
  | Trans.T s::ts -> Trans.U s::(undo_of ts) 
  | t::ts -> t::(undo_of ts)
  
let undoable n =
  let n = copy n in
  (TransSet.iter
    (fun t ->
      let undo_t = undo_of t in
      add_trans undo_t n;
      let inputs_of_t = inputs_of_trans t n in
      let outputs_of_t = outputs_of_trans t n in
      (PlaceSet.iter
        (fun p -> add_to_trans_arc p undo_t n)
        outputs_of_t
      );
      (PlaceSet.iter
        (fun p -> add_to_place_arc undo_t p n)
        inputs_of_t
      )
    )
    (transitions n)
  );
  n
