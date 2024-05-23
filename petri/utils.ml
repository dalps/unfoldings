let spr = Printf.sprintf

(* Function composition
   g must be unary, f may take any number of arguments
   Arrows follow application order
*)
let ( << ) f g x = f (g x)
let ( >> ) f g = g << f

let apples_of_pears (type fruit apples pears)
    (module Apples : Set.S with type elt = fruit and type t = apples)
    (module Pears : Set.S with type elt = fruit and type t = pears) fruit =
  Pears.fold Apples.add fruit Apples.empty

let apples_of_pears_map (type fruit apples pears) (f : fruit -> fruit)
    (module Apples : Set.S with type elt = fruit and type t = apples)
    (module Pears : Set.S with type elt = fruit and type t = pears) fruit =
  Pears.fold (f >> Apples.add) fruit Apples.empty

let string_of_set (type s t)
    (module Set : Set.S with type elt = s and type t = t) string_of_elt set =
  let l = Set.elements set in
  match l with
  | [] -> "&empty;"
  | _ -> spr "{%s}" (String.concat ", " (List.map string_of_elt l))
