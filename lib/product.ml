type 'a global_t = [ `Idle | `T of 'a ] list

let is_idle t = t = `Idle
let participates i t = List.nth t i <> `Idle
let projection i h = List.filter (participates i) h

let is_independent t1 t2 =
  List.for_all (fun (l1, l2) -> is_idle l1 || is_idle l2) (List.combine t1 t2)

module Make (Net : Petrinet.S) = struct
  module GlobalTransition = struct
    type t = Net.trans global_t

    let compare = compare
  end

  include Petrinet.Make (Net.Place) (GlobalTransition)

  let product ns sync =
    let placeset_of net_placeset =
      Net.PlaceSet.fold PlaceSet.add net_placeset PlaceSet.empty
    in
    of_sets
      (List.fold_right
         (fun n -> PlaceSet.union (placeset_of (Net.places n)))
         ns PlaceSet.empty)
      (TransSet.of_list sync)
      (fun global_t ->
        List.fold_right2
          (fun n local_t ->
            PlaceSet.union
              (match local_t with
              | `Idle -> PlaceSet.empty
              | `T t -> placeset_of (Net.preset_t n t)))
          ns global_t PlaceSet.empty)
      (fun global_t ->
        List.fold_right2
          (fun n local_t ->
            PlaceSet.union
              (match local_t with
              | `Idle -> PlaceSet.empty
              | `T t -> placeset_of (Net.postset_t n t)))
          ns global_t PlaceSet.empty)
      (List.fold_right
         (fun n -> PlaceSet.union (placeset_of (Net.marking n)))
         ns PlaceSet.empty)
end
