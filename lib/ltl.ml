module Make (AP : Set.OrderedType) = struct
  module APSet = Set.Make (AP)

  type t =
    | True
    | False
    | Atom of AP.t
    | And of t * t
    | Not of t
    | X of t
    | U of t * t

  let rec length = function
    | True | False | Atom _ -> 0
    | Not t | X t -> 1 + length t
    | And (t1, t2) | U (t1, t2) -> 1 + length t1 + length t2

  let eval s f =
    (* now s f checks whether s |= f *)
    let rec now s f =
      match s with
      | [] -> true
      | a0 :: atail -> (
          match f with
          | True -> true
          | False -> false
          | Atom a -> APSet.mem a a0
          | Not f' -> not (now s f')
          | And (f1, f2) -> now s f1 && now s f2
          | X f' -> now atail f'
          | U (f1, f2) -> (
              match eventually s f2 0 with
              | None -> false
              | Some j -> always s f1 j))
    (* eventually s f j checks whether there exists j such that s[j...] |= f *)
    and eventually s f j =
      match s with
      | [] -> None
      | _ :: atail -> if now s f then Some j else eventually atail f (j + 1)
    (* always s f j checks whether s[i...] |= f for 0 <= i < j*)
    and always s f j =
      match s with
      | [] -> false
      | _ :: atail -> j = 0 || (j > 0 && now s f && always atail f (j - 1))
    in
    now s f
end
