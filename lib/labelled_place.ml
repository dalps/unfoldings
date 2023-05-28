open Product_transition

type t = {
  history : Product_transition.t list;
  label : State.t
}

let build history label = {history; label}

let history p = p.history

let label p = p.label

let compare p1 p2 = 
  (* temporary strategy *)
  let n = Product_transition.sl_compare p1.history p2.history in
  if n = 0 then
    compare p1.label p2.label
  else n
