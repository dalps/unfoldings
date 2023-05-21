type t = string

exception IllegalGlobalTransition

let __SEP__ = ','
let __IDLE__ = '_'

let sep = Char.escaped __SEP__
let idle = Char.escaped __IDLE__

let represents_local_transition t =
  not (String.contains t __SEP__) &&
  not (String.contains t __IDLE__)

let is_idle t = String.contains t __IDLE__

let explode = String.split_on_char __SEP__

let is_well_formed t =
  let rec helper l = match l with
      [] -> false
    | s::ss -> if String.contains s __IDLE__ 
        then s = idle && helper ss 
        else helper ss

  in helper (explode t)

let participates i t = List.nth (explode t) i <> Char.escaped __IDLE__

let projection i h = List.filter (participates i) h

let is_independent t1 t2 = List.for_all
  (fun (le1,le2) -> is_idle le1 || is_idle le2)
  (List.combine (explode t1) (explode t2))

(* Two transition words are equivalent if one can be obtained from the other
   by swapping consecutive indepentent transitions. *)
let rec tword_equiv w1 w2 = match w1,w2 with
| [], [] -> true
| [], _::_ | _::_, [] -> false 
| t1::u1::w1', u2::t2::w2' when t1 = t2 && u1 <> u2 && is_independent u2 t2 -> 
    tword_equiv (u1::w1') (u2::w2')
| t1::u1::w1', u2::t2::w2' when u1 = u2 && t1 <> t2 && is_independent t1 u1 -> 
    tword_equiv (t1::w1') (t2::w2')
| t1::u1::w1', u2::t2::w2' when t1 = t2 && u1 = u2 && is_independent t1 u1 -> 
    tword_equiv (u1::w1') (u2::w2')
| a1::w1', a2::w2' -> a1 = a2 && tword_equiv w1' w2'

(* trace w is the list of all words w' such that w tword_eq uiv w' (includes w) *)
let trace w =
  let combine ws vs =
    List.fold_left
    (fun traces w ->
      (List.fold_left
      (fun traces v -> [w @ v] @ traces)
      []
      vs) @ traces)
    []
    ws
  in
  
  let rec scramble = function
      [] -> []
    | [t] -> [[t]]
    | t::u::r::v' when (is_independent t u) && (is_independent t r) ->
        combine [[t]] (scramble (u::r::v')) @
        combine [[u;t]] (scramble (r::v')) (* exclude t to prevent swapping it with non-consecutive independents (like r) *)
    | t::u::v' when is_independent t u ->
        combine [[t]] (scramble (u::v')) @
        combine [[u]] (scramble (t::v'))
    | t::v' -> combine [[t]] (scramble v')

  in scramble w

let concat_traces w w' = trace (w @ w')

let to_alpha t = String.concat "" (List.filter (fun u -> not (is_idle u)) (explode t))

let to_alpha_word w = String.concat "" (List.map to_alpha w)

let sl_compare w w' =
  let len_diff = List.length w - List.length w' in
  if len_diff = 0 then
    String.compare (to_alpha_word w) (to_alpha_word w')
  else len_diff

let projections w =
  let len = List.length (explode (List.hd w)) in
  assert(List.for_all (fun t -> List.length (explode t) = len) w);

  let rec helper i projs =
    if i = len then projs
    else helper (i+1) (projs @ [(projection i w)])

  in helper 0 []

let d_compare cmp w w' =
  List.fold_left2
    (fun res wk wk' ->
      let diff = cmp wk wk' in
      (* Compute the difference between the k-th projections, with k in 1..n;
         once it is not 0, carry it over the next steps as the final result. *)
      if res <> 0 then res else diff)
    0
    (projections w)
    (projections w')
    
let compare = compare