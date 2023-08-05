module Make (AP : Set.OrderedType) = struct
  module APSet = Set.Make (AP)

  type t =
    | True
    | False
    | AP of AP.t
    | Or of t * t
    | And of t * t
    | Not of t
    | X of t
    | U of t * t

  let rec length = function
    | True | False | AP _ -> 0
    | Not t | X t -> 1 + length t
    | And (t1, t2) | Or (t1, t2) | U (t1, t2) -> 1 + length t1 + length t2

  let rec eval s f =
    match s with
    | [] -> false
    | a0 :: s1 -> (
        match f with
        | True -> true
        | False -> false
        | AP a -> APSet.mem a a0
        | Or (f1, f2) -> eval s f1 || eval s f2
        | And (f1, f2) -> eval s f1 && eval s f2
        | Not f' -> not (eval s f')
        | X f' -> eval s1 f'
        | U (True, f2) -> eval s (Or (f2, X f))
        | U (f1, f2) -> eval s (Or (f2, And (f1, X f))))
end
