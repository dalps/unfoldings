open Product_transition

type t = {
  name : int;
  history : Product_transition.t list;
  label : Product_transition.t
}

let build name history label = {name; history; label}

let name e = e.name

let history e = e.history

let label e = e.label
  
let compare e1 e2 =
  (* temporary strategy *)
  if tword_equiv e1.history e2.history then
    0
  else
    let n = sl_compare e1.history e2.history in
    if n = 0 then
      compare e1.label e2.label
    else n