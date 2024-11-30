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

module Make (Ord : Set.OrderedType) = struct
  module Self = Set.Make (Ord)
  include Self

  let lift_map (type e tgt) (f : elt -> e)
      (module Target : Set.S with type elt = e and type t = tgt) =
    lift_map f (module Self) (module Target)

  let lift (type tgt)
      (module Target : Set.S with type elt = elt and type t = tgt) =
    lift_map Fun.id (module Target)

  (* bind : 'a t -> ('a -> 'b t) -> 'b t *)
  let bind (type e tgt)
      (module Target : Set.S with type elt = e and type t = tgt) result next =
    Self.fold (fun x acc -> Target.union (next x) acc) result Target.empty

  let return = singleton

  let ( let* ) = bind

  let ( let+ ) (type e tgt)
      (module Target : Set.S with type elt = e and type t = tgt) s f =
    lift_map f (module Target) s

  let ( and+ ) (type e tgt both)
      (module Target : Set.S with type elt = e and type t = tgt)
      (module Both : Set.S with type elt = elt * e and type t = both) a b =
    let xs = Self.to_seq a in
    let ys = Target.to_seq b in
    Seq.zip xs ys |> Both.of_seq

  let powerset_bad (type tgt) (module P : Set.S with type elt = t and type t = tgt)
      =
    let rec go s =
      match Self.elements s with
      | [] -> P.singleton Self.empty
      | x :: xs ->
          let p = go (Self.of_list xs) in
          P.union p (P.map (Self.add x) p)
    in
    go

  let powerset (type tgt) (module P : Set.S with type elt = t and type t = tgt)
      s =
    fold
      (fun x acc -> P.union acc (P.map (Self.add x) acc))
      s (P.singleton Self.empty)
end

(*

let get_set_monad (type a) (module Elt : Set.OrderedType with type t = a) :
    (module Monad) =
  (module struct
    module S = Set.Make (Elt)

    type t = S.t

    let return = S.singleton

  end)

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
