let spr = Printf.sprintf

(* Function composition
   g must be unary, f may take any number of arguments
   Arrows follow application order
*)
let ( << ) f g x = f (g x)
let ( >> ) f g = g << f

module Generalized = struct
  let lift_map (type fruit veggies apples onions) (f : fruit -> veggies)
      (module Apples : Set.S with type elt = fruit and type t = apples)
      (module Onions : Set.S with type elt = veggies and type t = onions) fruit
      =
    Apples.fold (f >> Onions.add) fruit Onions.empty

  let lift (type fruit apples pears)
      (module Apples : Set.S with type elt = fruit and type t = apples)
      (module Pears : Set.S with type elt = fruit and type t = pears) =
    lift_map Fun.id (module Apples) (module Pears)

  let return (type x xs) (module X : Set.S with type elt = x and type t = xs) =
    X.singleton

  let bind (type x y xs ys) (module X : Set.S with type elt = x and type t = xs)
      (module Y : Set.S with type elt = y and type t = ys) result next =
    X.fold (fun x acc -> Y.union (next x) acc) result Y.empty

  let prod (type x y xs ys xys)
      (module X : Set.S with type elt = x and type t = xs)
      (module Y : Set.S with type elt = y and type t = ys)
      (module XY : Set.S with type elt = x * y and type t = xys) xs ys =
    let xs, ys = (X.to_seq xs, Y.to_seq ys) in
    Seq.zip xs ys |> XY.of_seq

  let string_of_set (type e t)
      (module Set : Set.S with type elt = e and type t = t) ?(empty = "{}")
      ?(separator = ",") ?(lbracket = "{") ?(rbracket = "}") ~string_of_elt set
      =
    Set.elements set |> function
    | [] -> empty
    | l ->
        spr "%s%s%s" lbracket
          (String.concat separator (List.map string_of_elt l))
          rbracket
end

module type S = sig
  include Set.S

  val lift_map :
    (elt -> 'e) ->
    (module Set.S with type elt = 'e and type t = 'tgt) ->
    t ->
    'tgt

  val lift : (module Set.S with type elt = elt and type t = 'tgt) -> t -> 'tgt

  val return : elt -> t

  val bind :
    (module Set.S with type elt = 'e and type t = 'tgt) ->
    t ->
    (elt -> 'tgt) ->
    'tgt

  val ( |- ) :
    (module Set.S with type elt = 'e and type t = 'tgt) ->
    t ->
    (elt -> 'tgt) ->
    'tgt

  val prod :
    (module Set.S with type elt = 'e and type t = 'tgt) ->
    (module Set.S with type elt = elt * 'e and type t = 'both) ->
    t ->
    'tgt ->
    'both

  val powerset : (module Set.S with type elt = t and type t = 'tgt) -> t -> 'tgt

  val combinations :
    (module Set.S with type elt = t and type t = 'tgt) -> 'tgt -> 'tgt

  val to_string : elt:(elt -> string) -> t -> string
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t = struct
  module Self = Set.Make (Ord)
  include Self

  let lift_map (type e tgt) (f : elt -> e)
      (module Target : Set.S with type elt = e and type t = tgt) =
    Generalized.lift_map f (module Self) (module Target)

  let lift (type tgt)
      (module Target : Set.S with type elt = elt and type t = tgt) =
    lift_map Fun.id (module Target)

  let return = singleton

  let bind (type e tgt)
      (module Target : Set.S with type elt = e and type t = tgt) =
    Generalized.bind (module Self) (module Target)

  let ( |- ) = bind (* use with [@@] *)

  let prod (type e tgt)
      (module Target : Set.S with type elt = e and type t = tgt) =
    Generalized.prod (module Self) (module Target)

  let powerset (type tgt) (module P : Set.S with type elt = t and type t = tgt)
      s =
    fold
      (fun x acc -> P.union acc (P.map (Self.add x) acc))
      s (P.singleton Self.empty)

  let combinations (type tgt)
      (module P : Set.S with type elt = t and type t = tgt) s =
    let combine1 (ps : Self.t) (result : P.t) =
      bind (module P) ps @@ fun p ->
      Generalized.bind (module P) (module P) result @@ fun set ->
      Generalized.return (module P) (Self.add p set)
    in

    Seq.init (P.cardinal s) (fun _ -> Self.empty)
    |> P.of_seq |> P.fold combine1 s

  let to_string ~elt t =
    Generalized.string_of_set (module Self) ~string_of_elt:elt t
end

(*

open Ints
let foo s =
  let* x (module Strings) = s in
  Strings.return (string_of_int x)

let local (type e tgt) (module M : Set.S with type elt = e and type t = tgt) : unit
    =
  let open M in
  ()

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
