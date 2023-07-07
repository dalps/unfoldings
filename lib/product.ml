type local_t = Idle | T of string

module GlobalTransition = struct
  type t = local_t list

  let is_idle = ( = ) Idle
  let participates i t = List.nth t i <> Idle
  let projection i h = List.filter (participates i) h

  let is_independent t1 t2 =
    List.for_all (fun (l1, l2) -> is_idle l1 || is_idle l2) (List.combine t1 t2)

  let compare = compare
end

include Petrinet.Make (String) (GlobalTransition)

let product ns sync =
  of_sets
    (List.fold_right (fun n -> PlaceSet.union (places n)) ns PlaceSet.empty)
    (TransSet.of_list sync)
    (fun global_t ->
      List.fold_right2
        (fun n local_t -> PlaceSet.union (preset_t n [ local_t ]))
        ns global_t PlaceSet.empty)
    (fun global_t ->
      List.fold_right2
        (fun n local_t -> PlaceSet.union (postset_t n [ local_t ]))
        ns global_t PlaceSet.empty)
    (List.fold_right (fun n -> PlaceSet.union (marking n)) ns PlaceSet.empty)
