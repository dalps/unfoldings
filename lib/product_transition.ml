type t = string

exception IllegalGlobalTransition

let __SEP__ = ','
let __IDLE__ = '_'

let sep = Char.escaped __SEP__
let idle = Char.escaped __IDLE__

let represents_local_transition l =
  not (String.contains l __SEP__) &&
  not (String.contains l __IDLE__)

let is_idle = (=) idle

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
  (fun (le1,le2) -> is_idle le1 <> is_idle le2)
  (List.combine (explode t1) (explode t2))

(* Two transition words are equivalent if on can be obtained from the other
    by swapping consecutive indepentent transitions.
    Transition words are lists of string-encoded Events. *)
let rec is_equivalent w1 w2 = match w1,w2 with
| [], [] -> true
| [], _::_ | _::_, [] -> false 
| t1::u1::w1', u2::t2::w2' when t1 = t2 && u1 = u2 -> 
    is_independent t1 u1 && is_equivalent w1' w2'
| a1::w1', a2::w2' -> a1 = a2 && is_equivalent w1' w2'

let compare = compare