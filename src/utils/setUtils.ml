let spr = Printf.sprintf

(* Function composition
   g must be unary, f may take any number of arguments
   Arrows follow application order
*)
let ( << ) f g x = f (g x)
let ( >> ) f g = g << f

let lift_map (type fruit veggies apples onions) (f : fruit -> veggies)
    (module Apples : Set.S with type elt = fruit and type t = apples)
    (module Onions : Set.S with type elt = veggies and type t = onions) fruit =
  Apples.fold (f >> Onions.add) fruit Onions.empty

let lift (type fruit apples pears)
    (module Apples : Set.S with type elt = fruit and type t = apples)
    (module Pears : Set.S with type elt = fruit and type t = pears) =
  lift_map Fun.id (module Apples) (module Pears)

let string_of_set (type e t)
    (module Set : Set.S with type elt = e and type t = t) ?(empty = "{}")
    string_of_elt set =
  Set.elements set |> function
  | [] -> empty
  | l -> spr "{%s}" (String.concat ", " (List.map string_of_elt l))

(*
module type StringRepr = sig
  type t
  val to_string : t -> string
end

let string_of_set (type e t)
    (module Elt : StringRepr with type t = e)
    (module Set : Set.S with type elt = e and type t = t) ?(empty="&empty;") set =
  let l = Set.elements set in
  match l with
  | [] -> empty
  | _ -> spr "{%s}" (String.concat ", " (List.map Elt.to_string l))
*)
