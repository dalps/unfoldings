module Make (AP : Set.OrderedType) = struct
  module APSet = Set.Make (AP)

  module Formula = struct
    type t =
      | True
      | False
      | AP of AP.t
      | Or of t * t
      | And of t * t
      | Not of t
      | X of t
      | U of t * t

    let compare f1 f2 =
      match (f1, f2) with
      | f1, Not (Not f2) | Not (Not f1), f2 -> Stdlib.compare f1 f2
      | f1, f2 -> Stdlib.compare f1 f2
  end

  module FormulaSet = Set.Make (Formula)

  let rec length = function
    | Formula.True | False | AP _ -> 0
    | Not t | X t -> 1 + length t
    | And (t1, t2) | Or (t1, t2) | U (t1, t2) -> 1 + length t1 + length t2

  let rec eval s f =
    match s with
    | [] -> false
    | a0 :: s1 -> (
        match f with
        | Formula.True -> true
        | False -> false
        | AP a -> APSet.mem a a0
        | Or (f1, f2) -> eval s f1 || eval s f2
        | And (f1, f2) -> eval s f1 && eval s f2
        | Not f' -> not (eval s f')
        | X f' -> eval s1 f'
        | U (True, f2) -> eval s (Or (f2, X f))
        | U (f1, f2) -> eval s (Or (f2, And (f1, X f))))

  module PowerAPSet = Set.Make (APSet)

  let rec powerset apset =
    match APSet.elements apset with
    | [] -> PowerAPSet.singleton APSet.empty
    | a :: aps' ->
        let p = powerset (APSet.of_list aps') in
        PowerAPSet.union p (PowerAPSet.map (fun x -> APSet.add a x) p)

  let labels_of_formula aps f =
    PowerAPSet.filter (fun apset -> eval [ apset ] f) (powerset aps)

  let ( #@ ) l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 @ l2

  let rec closure f =
    FormulaSet.union
      (FormulaSet.of_list [ f; Not f ])
      (match f with
      | Formula.True | False | AP _ -> FormulaSet.empty
      | Not f' | X f' -> closure f'
      | And (f1, f2) | Or (f1, f2) | U (f1, f2) ->
          FormulaSet.union (closure f1) (closure f2))
end
