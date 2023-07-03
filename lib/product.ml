module Trans = struct
  type local_t = Idle | T of string

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
  let preset gt =
    List.fold_right2
      (fun n t -> PlaceSet.union (preset_t n [t])) ns gt PlaceSet.empty
  in
  let postset gt =
    List.fold_right2
      (fun n t -> PlaceSet.union (postset_t n [t])) ns gt PlaceSet.empty
  in
  of_sets
    (List.fold_right (fun n -> PlaceSet.union (places n)) ns PlaceSet.empty)
    (TransSet.of_list sync)
    preset
    postset
    (List.fold_right (fun n -> PlaceSet.union (marking n)) ns PlaceSet.empty)
      