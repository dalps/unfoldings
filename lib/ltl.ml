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
  module PowerFormulaSet = Set.Make (FormulaSet)

  let rec powerapset apset =
    match APSet.elements apset with
    | [] -> PowerAPSet.singleton APSet.empty
    | a :: aps' ->
        let p = powerapset (APSet.of_list aps') in
        PowerAPSet.union p (PowerAPSet.map (fun x -> APSet.add a x) p)

  let rec powerformulaset apset =
    match FormulaSet.elements apset with
    | [] -> PowerFormulaSet.singleton FormulaSet.empty
    | a :: aps' ->
        let p = powerformulaset (FormulaSet.of_list aps') in
        PowerFormulaSet.union p
          (PowerFormulaSet.map (fun x -> FormulaSet.add a x) p)

  let labels_of_formula aps f =
    PowerAPSet.filter (fun apset -> eval [ apset ] f) (powerapset aps)

  let ( #@ ) l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 @ l2

  let rec closure f =
    FormulaSet.union
      (FormulaSet.of_list [ f; Not f ])
      (match f with
      | Formula.True | False | AP _ -> FormulaSet.empty
      | Not f' | X f' -> closure f'
      | And (f1, f2) | Or (f1, f2) | U (f1, f2) ->
          FormulaSet.union (closure f1) (closure f2))

  let ( => ) x y = (not x) || y
  let ( <=> ) x y = x => y && y => x

  let elementary_sets f =
    let cl = closure f in
    let pcl = powerformulaset cl in
    let is_logically_consistent b =
      FormulaSet.mem True cl => FormulaSet.mem True b
      && FormulaSet.for_all
           (fun g -> FormulaSet.mem g b => not (FormulaSet.mem (Not g) b))
           cl
      && FormulaSet.for_all
           (function
             | And (g1, g2) as g ->
                 FormulaSet.mem g b
                 <=> (FormulaSet.mem g1 b && FormulaSet.mem g2 b)
             | _ -> true)
           cl
    in
    let is_locally_consistent b =
      FormulaSet.for_all
        (function
          | U (g1, g2) as g ->
              FormulaSet.mem g2 b => FormulaSet.mem g b
              && (FormulaSet.mem g b && not (FormulaSet.mem g2 b))
                 => FormulaSet.mem g1 b
          | _ -> true)
        cl
    in
    let is_maximal b =
      FormulaSet.for_all
        (fun g -> (not (FormulaSet.mem g b)) => FormulaSet.mem (Not g) b)
        cl
    in

    PowerFormulaSet.filter
      (fun b ->
        is_logically_consistent b && is_locally_consistent b && is_maximal b)
      pcl

  module FormulaGNBA = Gnba.Make (FormulaSet) (APSet)

  let gnba_of_formula ap f =
    let cl = closure f in
    let states = elementary_sets f in
    let formulas_of apset =
      APSet.fold (fun a -> FormulaSet.add (AP a)) apset FormulaSet.empty
    in
    let formulas_of_ap = formulas_of ap in
    FormulaGNBA.of_sets states (powerapset ap)
      (fun b a ->
        if
          FormulaSet.equal
            (FormulaSet.inter (formulas_of a) cl)
            (FormulaSet.inter formulas_of_ap b)
        then
          PowerFormulaSet.filter
            (fun b' ->
              FormulaSet.for_all
                (function
                  | X g' as g -> FormulaSet.mem g b <=> FormulaSet.mem g' b'
                  | U (g1, g2) as g ->
                      FormulaSet.mem g b
                      <=> (FormulaSet.mem g2 b
                          || (FormulaSet.mem g1 b && FormulaSet.mem g b'))
                  | _ -> true)
                cl)
            states
        else PowerFormulaSet.empty)
      (PowerFormulaSet.filter (FormulaSet.mem f) states)
      (FormulaSet.fold
         (function
           | U (_, g2) as g ->
               PowerFormulaSet.union
                 (PowerFormulaSet.filter
                    (fun b -> (not (FormulaSet.mem g b)) || FormulaSet.mem g2 b)
                    states)
           | _ -> PowerFormulaSet.union PowerFormulaSet.empty)
         cl PowerFormulaSet.empty)
end
