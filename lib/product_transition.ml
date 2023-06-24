type local_t = Idle | T of string

type t = local_t list

let rec string_of_t = function
  | [] -> ""
  | [Idle] -> "_"
  | [T s] -> s
  | Idle::ls -> "_," ^ string_of_t ls
  | T s::ls -> s ^ "," ^ string_of_t ls

let is_idle = (=) Idle

let participates i t = List.nth t i <> Idle

let projection i h = List.filter (participates i) h

let is_independent t1 t2 = List.for_all
  (fun (l1, l2) -> is_idle l1 || is_idle l2)
  (List.combine t1 t2)

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

let sl_compare w w' =
  let string_of_t t = 
    String.concat "" (
      List.map (function
        | Idle -> ""
        | T s -> s
      )
      t
    )
  in
  let string_of_tword w = String.concat "" (List.map string_of_t w) in
  let len_diff = List.length w - List.length w' in
  if len_diff = 0 then
    String.compare (string_of_tword w) (string_of_tword w')
  else len_diff

let projections w =
  let len = List.length (List.hd w) in
  assert(
    let cond = List.for_all (fun t -> List.length t = len) w in
    if not cond then 
      print_endline "All global transitions must have the same length.";
    cond
  );
  let rec helper i projs =
    if i = len then projs
    else helper (i+1) (projs @ [(projection i w)])
  in helper 0 []

let d_compare cmp w w' =
  List.fold_left2
    (fun res wk wk' ->
      let diff = cmp wk wk' in
      (* Compute the difference between the k-th projections, with k = 1..n;
         once it is not 0, carry it over the next steps as the final result. *)
      if res <> 0 then res else diff)
    0
    (projections w)
    (projections w')
    
let compare = compare