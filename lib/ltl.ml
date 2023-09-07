type 'a formula =
  | True
  | False
  | AP of 'a
  | Or of 'a formula * 'a formula
  | And of 'a formula * 'a formula
  | Not of 'a formula
  | If of 'a formula * 'a formula
  | Iff of 'a formula * 'a formula
  | X of 'a formula
  | U of 'a formula * 'a formula
  | F of 'a formula
  | G of 'a formula

let string_of_formula string_of_ap f =
  let rec helper = function
    | True -> "true"
    | False -> "false"
    | AP a -> string_of_ap a
    | Not (AP _ as f) -> "¬" ^ helper f
    | Not f -> "¬(" ^ helper f ^ ")"
    | X f -> "X " ^ helper f
    | F f -> "F " ^ helper f
    | G f -> "G " ^ helper f
    | If (f1, f2) -> helper f1 ^ " => " ^ helper f2
    | Iff (f1, f2) -> helper f1 ^ " <=> " ^ helper f2
    | Or (f1, f2) -> helper f1 ^ " ∨ " ^ helper f2
    | And (f1, f2) -> helper f1 ^ " ∧ " ^ helper f2
    | U (f1, f2) -> helper f1 ^ " U " ^ helper f2
  in
  helper f

module Make (AP : Set.OrderedType) = struct
  module APSet = Set.Make (AP)

  module Formula = struct
    type t = AP.t formula

    let compare f1 f2 =
      match (f1, f2) with
      | False, Not True | Not True, False -> 0
      | True, Not False | Not False, True -> 0
      | f1, Not (Not f2) | Not (Not f1), f2 -> Stdlib.compare f1 f2
      | f1, f2 -> Stdlib.compare f1 f2
  end

  module FormulaSet = Set.Make (Formula)

  let rec length = function
    | True | False | AP _ -> 0
    | Not t | X t | F t | G t -> 1 + length t
    | And (t1, t2) | Or (t1, t2) | U (t1, t2) | If (t1, t2) | Iff (t1, t2) ->
        1 + length t1 + length t2

  let ( => ) x y = (not x) || y
  let ( <=> ) x y = x => y && y => x

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
        | If (f1, f2) -> eval s f1 => eval s f2
        | Iff (f1, f2) -> eval s f1 <=> eval s f2
        | X f' -> eval s1 f'
        | G f' -> eval s (Not (F (Not f')))
        | U (True, f2) | F f2 -> eval s (Or (f2, X f))
        | U (f1, f2) -> eval s (Or (f2, And (f1, X f))))

  let rec expand = function
    | (True | False | AP _) as f -> f
    | Or (True, _) | Or (_, True) | Not False -> True
    | And (False, _) | And (_, False) | Not True -> False
    | Or (False, f) | Or (f, False) | And (True, f) | And (f, True) -> expand f
    | Or (f1, f2) -> Or (expand f1, expand f2)
    | And (f1, f2) -> And (expand f1, expand f2)
    | Not (Not f') -> expand f'
    | Not f' -> Not (expand f')
    | X f' -> X (expand f')
    | If (f1, f2) -> expand (Or (Not f1, f2))
    | Iff (f1, f2) -> Iff (expand f1, expand f2)
    | F (F f') -> expand (F f')
    | F f' -> expand (U (True, f'))
    | G (G f') -> expand (G f')
    | G f' -> expand (Not (F (Not f')))
    | U (f1, U (f1', f2)) when Formula.compare f1 f1' = 0 -> expand (U (f1, f2))
    | U (U (f1, f2), f2') when Formula.compare f2 f2' = 0 -> expand (U (f1, f2))
    | U (f1, f2) -> U (expand f1, expand f2)

  module PowerAPSet = Set.Make (APSet)
  module PowerFormulaSet = Set.Make (FormulaSet)

  let rec power_apset apset =
    match APSet.elements apset with
    | [] -> PowerAPSet.singleton APSet.empty
    | a :: aps' ->
        let p = power_apset (APSet.of_list aps') in
        PowerAPSet.union p (PowerAPSet.map (fun x -> APSet.add a x) p)

  let rec power_formulaset apset =
    match FormulaSet.elements apset with
    | [] -> PowerFormulaSet.singleton FormulaSet.empty
    | a :: aps' ->
        let p = power_formulaset (FormulaSet.of_list aps') in
        PowerFormulaSet.union p
          (PowerFormulaSet.map (fun x -> FormulaSet.add a x) p)

  let labels_of_formula ap f =
    PowerAPSet.filter (fun apset -> eval [ apset ] f) (power_apset ap)

  let closure g =
    let rec helper f =
      FormulaSet.union
        (FormulaSet.of_list
           (match f with
           | True | Not False -> [ True ]
           | False | Not True -> [ False ]
           | Not f' -> [ f; f' ]
           | _ -> [ f; Not f ]))
        (match f with
        | True | False | AP _ -> FormulaSet.empty
        | Not f' | X f' -> helper f'
        | And (f1, f2) | Or (f1, f2) | U (f1, f2) ->
            FormulaSet.union (helper f1) (helper f2)
        | _ -> FormulaSet.empty)
    in
    helper (expand g)

  let ap_of_formulaset b =
    FormulaSet.fold
      (fun f b' -> match f with AP ap -> APSet.add ap b' | _ -> b')
      b APSet.empty

  let ap_of_formula f = ap_of_formulaset (closure f)

  let ap_of_formulaset_f b =
    FormulaSet.filter_map (function AP _ as f -> Some f | _ -> None) b

  let elementary_sets f =
    let cl = closure f in
    PowerFormulaSet.filter
      (fun b ->
        (FormulaSet.for_all (function
          | True -> FormulaSet.mem True b
          | Not g' as g -> FormulaSet.mem g b <=> not (FormulaSet.mem g' b)
          | Or (g1, g2) as g ->
              FormulaSet.mem g b <=> (FormulaSet.mem g1 b || FormulaSet.mem g2 b)
          | And (g1, g2) as g ->
              FormulaSet.mem g b <=> (FormulaSet.mem g1 b && FormulaSet.mem g2 b)
          | If (g1, g2) as g ->
              FormulaSet.mem g b <=> (FormulaSet.mem g1 b => FormulaSet.mem g2 b)
          | Iff (g1, g2) as g ->
              FormulaSet.mem g b
              <=> (FormulaSet.mem g1 b <=> FormulaSet.mem g2 b)
          | U (g1, g2) as g ->
              FormulaSet.mem g2 b => FormulaSet.mem g b
              && (FormulaSet.mem g b && not (FormulaSet.mem g2 b))
                 => FormulaSet.mem g1 b
          | _ -> true))
          cl)
      (power_formulaset cl)

  module FormulaGNBA = Gnba.Make (FormulaSet) (APSet)

  let gnba_of_formula ap f =
    let cl = closure f in
    let states = elementary_sets f in
    let formulas_of apset =
      APSet.fold (fun a -> FormulaSet.add (AP a)) apset FormulaSet.empty
    in
    let formulas_of_ap = formulas_of ap in
    let open FormulaGNBA in
    of_sets states (power_apset ap)
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
               PowerStateSet.add
                 (PowerFormulaSet.filter
                    (fun b -> (not (FormulaSet.mem g b)) || FormulaSet.mem g2 b)
                    states)
           | _ -> PowerStateSet.union PowerStateSet.empty)
         cl PowerStateSet.empty)

  let nba_of_formula ap f = FormulaGNBA.to_nba (gnba_of_formula ap f)

  module NumberedAPSet = struct
    type t = int * APSet.t

    let label = snd
    let compare = compare
  end

  module FormulaPTNet =
    Petrinet.Make (FormulaGNBA.NumberedState) (NumberedAPSet)

  type flow = {
    source : FormulaPTNet.place;
    label : APSet.t;
    target : FormulaPTNet.place;
  }

  let petrinet_of_formula ap f =
    let b = nba_of_formula ap f in
    let powerapset = power_apset ap in
    (* awful construction *)
    let flow =
      PowerAPSet.fold
        (fun label ->
          FormulaGNBA.NumberedNba.StateSet.fold
            (fun source ->
              FormulaGNBA.NumberedNba.StateSet.fold
                (fun target -> List.cons { source; label; target })
                (b.func source label))
            b.states)
        powerapset []
    in
    FormulaPTNet.of_sets b.states
      (FormulaPTNet.TransSet.of_list (List.mapi (fun i r -> (i, r.label)) flow))
      (fun (i, apset) ->
        let r = List.nth flow i in
        if APSet.equal apset r.label then
          FormulaPTNet.PlaceSet.singleton r.source
        else FormulaPTNet.PlaceSet.empty)
      (fun t ->
        let r = List.nth flow (fst t) in
        FormulaPTNet.PlaceSet.singleton r.target)
      b.init
end
